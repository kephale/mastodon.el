;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (persist "0.4"))
;; Homepage: https://codeberg.org/martianh/mastodon.el

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mastodon-notification.el provides notification functions for Mastodon.

;;; Code:

(require 'persist) ; for saving notifications count

(autoload 'mastodon-http--api "mastodon-http.el")
(autoload 'mastodon-http--post "mastodon-http.el")
(autoload 'mastodon-http--triage "mastodon-http.el")
(autoload 'mastodon-http--get-params-async-json "mastodon-http.el")
(autoload 'mastodon-media--inline-images "mastodon-media.el")
(autoload 'mastodon-tl--byline "mastodon-tl.el")
(autoload 'mastodon-tl--byline-author "mastodon-tl.el")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl.el")
(autoload 'mastodon-tl--content "mastodon-tl.el")
(autoload 'mastodon-tl--field "mastodon-tl.el")
(autoload 'mastodon-tl--find-property-range "mastodon-tl.el")
(autoload 'mastodon-tl--has-spoiler "mastodon-tl.el")
(autoload 'mastodon-tl--init "mastodon-tl.el")
(autoload 'mastodon-tl--init-sync "mastodon-tl.el")
(autoload 'mastodon-tl--insert-status "mastodon-tl.el")
(autoload 'mastodon-tl--property "mastodon-tl.el")
(autoload 'mastodon-tl--spoiler "mastodon-tl.el")
(autoload 'mastodon-tl--toot-id "mastodon-tl.el")
(autoload 'mastodon-http--get-params-async-json "mastodon-http.el")
(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--display-media-p)
(defvar mastodon-tl--buffer-spec)

;;;###autoload
(defgroup mastodon-notifications nil
  "Notifications in mastodon."
  :prefix "mastodon-notifications-"
  :group 'mastodon)

;;;###autoload
(defcustom mastodon-notifications-reload-when-new nil
  "Reload the notifications timeline when new notifications are found.
The check is done by `mastodon-notifications--check-for-new'. You
may want to disable this if you often get a lot of notifications at
once and the constant reloading is an interruption."
  :group 'mastodon-notifications
  :type 'boolean)

;;;###autoload
(defcustom mastodon-notifications-display-modeline-count t
  "Display unread notifications count in the modeline."
  :group 'mastodon-notifications
  :type 'boolean)


(defvar mastodon-notifications-modeline-indicator nil
  "Holds the display string to be added to `mode-line-misc-info'.")

(persist-defvar mastodon-notifications-newest-id nil
                "The ID of the newest notification already loaded and seen
  locally. It's value is saved between sessions by `persist'.")

(defvar mastodon-notifications--types-alist
  '(("mention" . mastodon-notifications--mention)
    ("follow" . mastodon-notifications--follow)
    ("favourite" . mastodon-notifications--favourite)
    ("reblog" . mastodon-notifications--reblog)
    ("follow_request" . mastodon-notifications--follow-request)
    ("status" . mastodon-notifications--status)
    ("poll" . mastodon-notifications--poll))
  "Alist of notification types and their corresponding function.")

(defvar mastodon-notifications--response-alist
  '(("Mentioned" . "you")
    ("Followed" . "you")
    ("Favourited" . "your status from")
    ("Boosted" . "your status from")
    ("Requested to follow" . "you")
    ("Posted" . "a post")
    ("Posted a poll" . "that has now ended"))
  "Alist of subjects for notification types.")

(defvar mastodon-notifications-new-notifications-timer nil
  "The timer object used to check for new notifications.")

(defun mastodon-notifications--byline-concat (message)
  "Add byline for TOOT with MESSAGE."
  (concat
   " "
   (propertize message 'face 'highlight)
   " "
   (cdr (assoc message mastodon-notifications--response-alist))))

(defun mastodon-notifications--follow-request-process (&optional reject)
  "Process the follow request at point.
With no argument, the request is accepted. Argument REJECT means
reject the request. Can be called in notifications view or in
follow-requests view."
  (interactive)
  (if (not (mastodon-tl--find-property-range 'toot-json (point)))
      (message "No follow request at point?")
    (let* ((toot-json (mastodon-tl--property 'toot-json))
           (f-reqs-view-p (string= "follow_requests"
                                   (plist-get mastodon-tl--buffer-spec 'endpoint)))
           (f-req-p (or (string= "follow_request" (alist-get 'type toot-json)) ;notifs
                        f-reqs-view-p)))
      (if f-req-p
          (let* ((account (or (alist-get 'account toot-json) ;notifs
                              toot-json)) ;f-reqs
                 (id (alist-get 'id account))
                 (handle (alist-get 'acct account))
                 (name (alist-get 'username account)))
            (if id
                (let ((response
                       (mastodon-http--post
                        (concat
                         (mastodon-http--api "follow_requests")
                         (format "/%s/%s" id (if reject
                                                 "reject"
                                               "authorize")))
                        nil nil)))
                  (mastodon-http--triage response
                                         (lambda ()
                                           (if f-reqs-view-p
                                               (mastodon-profile--view-follow-requests)
                                             (mastodon-notifications--get))
                                           (message "Follow request of %s (@%s) %s!"
                                                    name handle (if reject
                                                                    "rejected"
                                                                  "accepted")))))
              (message "No account result at point?")))
        (message "No follow request at point?")))))

(defun mastodon-notifications--follow-request-accept ()
  "Accept a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process))

(defun mastodon-notifications--follow-request-reject ()
  "Reject a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process t))

(defun mastodon-notifications--mention (note)
  "Format for a `mention' NOTE."
  (mastodon-notifications--format-note note 'mention))

(defun mastodon-notifications--follow (note)
  "Format for a `follow' NOTE."
  (mastodon-notifications--format-note note 'follow))

(defun mastodon-notifications--follow-request (note)
  "Format for a `follow-request' NOTE."
  (mastodon-notifications--format-note note 'follow-request))

(defun mastodon-notifications--favourite (note)
  "Format for a `favourite' NOTE."
  (mastodon-notifications--format-note note 'favourite))

(defun mastodon-notifications--reblog (note)
  "Format for a `boost' NOTE."
  (mastodon-notifications--format-note note 'boost))

(defun mastodon-notifications--status (note)
  "Format for a `status' NOTE.
Status notifications are given when
`mastodon-tl--enable-notify-user-posts' has been set."
  (mastodon-notifications--format-note note 'status))

(defun mastodon-notifications--poll (note)
  "Format for a `poll' NOTE."
  (mastodon-notifications--format-note note 'poll))

(defun mastodon-notifications--format-note (note type)
  "Format for a NOTE of TYPE."
  (let ((id (alist-get 'id note))
        (status (mastodon-tl--field 'status note))
        (follower (alist-get 'username (alist-get 'account note))))
    (mastodon-notifications--insert-status
     (cond ((or (equal type 'follow)
                (equal type 'follow-request))
            ;; Using reblog with an empty id will mark this as something
            ;; non-boostable/non-favable.
            (cons '(reblog (id . nil)) note))
           ;; reblogs/faves use 'note' to process their own json
           ;; not the toot's. this ensures following etc. work on such notifs
           ((or (equal type 'favourite)
                (equal type 'boost))
            note)
           (t
            status))
     (if (or (equal type 'follow)
             (equal type 'follow-request))
         (propertize (if (equal type 'follow)
                         "Congratulations, you have a new follower!"
                       (format "You have a follow request from... %s"
                               follower))
                       'face 'default)
       (mastodon-tl--clean-tabs-and-nl
        (if (mastodon-tl--has-spoiler status)
            (mastodon-tl--spoiler status)
          (mastodon-tl--content status))))
     (if (or (equal type 'follow)
             (equal type 'follow-request)
             (equal type 'mention))
         'mastodon-tl--byline-author
       (lambda (_status)
         (mastodon-tl--byline-author
          note)))
     (lambda (_status)
       (mastodon-notifications--byline-concat
        (cond ((equal type 'boost)
               "Boosted")
              ((equal type 'favourite)
               "Favourited")
              ((equal type 'follow-request)
               "Requested to follow")
              ((equal type 'follow)
               "Followed")
              ((equal type 'mention)
               "Mentioned")
              ((equal type 'status)
               "Posted")
              ((equal type 'poll)
               "Posted a poll"))))
     id
     (when (or (equal type 'favourite)
               (equal type 'boost))
       status))))

(defun mastodon-notifications--insert-status (toot body author-byline action-byline id &optional parent-toot)
  "Display the content and byline of timeline element TOOT.

BODY will form the section of the toot above the byline.

AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'.

ACTION-BYLINE is also an optional function for adding an action,
such as boosting favouriting and following to the byline. It also
takes a single function. By default it is
`mastodon-tl--byline-boosted'.

ID is the notification's own id, which is attached as a property.
If the status is a favourite or a boost, PARENT-TOOT is the JSON
of the toot responded to."
  (when toot ; handle rare blank notif server bug
    (mastodon-tl--insert-status toot body author-byline action-byline id parent-toot)))

(defun mastodon-notifications--by-type (note)
  "Filters NOTE for those listed in `mastodon-notifications--types-alist'."
  (let* ((type (mastodon-tl--field 'type note))
         (fun (cdr (assoc type mastodon-notifications--types-alist)))
         (start-pos (point)))
    (when fun
      (funcall fun note)
      (when mastodon-tl--display-media-p
        (mastodon-media--inline-images start-pos (point))))))

(defun mastodon-notifications--timeline (json)
  "Format JSON in Emacs buffer."
  (if (equal json '[])
      (message "Looks like you have no (more) notifications for the moment.")
    (mapc #'mastodon-notifications--by-type json)
    (goto-char (point-min))
    ;;set newest ID for notifications modeline alerts:
    (setq mastodon-notifications-newest-id
          (mastodon-tl--property 'toot-id))))

(defun mastodon-notifications--get ()
  "Display NOTIFICATIONS in buffer."
  (interactive)
  (message "Loading your notifications...")
  (mastodon-tl--init
   "notifications"
   "notifications"
   'mastodon-notifications--timeline))

(defun mastodon-notifications--masto-buffer-p (buffer)
  "Check if BUFFER is in `mastodon-mode' or `mastodon-toot-mode'."
  (or (eql major-mode 'mastodon-mode)
      ;; for profile update buffer, etc.:
      (string-prefix-p "*mastodon" (buffer-name buffer))
      (equal (buffer-name buffer) "*new toot*")))
;; doesn't work because our minor modes don't get killed!:
;; (member 'mastodon-toot-mode minor-mode-list)))

(defun mastodon-notifications--check-for-new-timer ()
  "Run `mastodon-notifications--check-for-new' with arg `mastodon-notifications-newest-id'."
  (mastodon-notifications--check-for-new mastodon-notifications-newest-id))

(defun mastodon-notifications--set-and-run-timer ()
  "Run a timer to check for new notifications.
First we cancel any existing timers to avoid them accumulating.
Run in `mastodon-mode-hook' if
`mastodon-notifications-display-modeline-count' is t."
  (when mastodon-notifications-new-notifications-timer
    (cancel-timer mastodon-notifications-new-notifications-timer))
  (setq mastodon-notifications-new-notifications-timer
        (run-at-time nil 5 #'mastodon-notifications--check-for-new-timer)))

(defun mastodon-notifications--check-for-new (newest-id)
  "Check the server for new notifications since NEWEST-ID.
Runs `mastodon-notifications--modeline-display-unread-count' on
the response."
  ;; only in masto modes:
  (when (mastodon-notifications--masto-buffer-p (current-buffer))
    (let ((prev-buffer (current-buffer)))
      (mastodon-http--get-params-async-json
       (mastodon-http--api "notifications")
       (lambda (status)
         (mastodon-notifications--modeline-display-unread-count status prev-buffer))
       nil
       t ;silent
       `(("since_id" . ,newest-id))))))

(defun mastodon-notifications--modeline-display-unread-count (response buffer)
  "If we have new notifications in RESPONSE, update modeline in BUFFER.
Callback for `mastodon-notifications--check-for-new'."
  (let* ((count (length response))
         (notifs-display (propertize
                          (concat (if (require 'all-the-icons nil :no-error)
                                      ""
                                    "notifs:")
                                  (number-to-string count))
                          'face 'mastodon-display-name-face
                          'mouse-face 'mode-line-highlight
                          'help-echo "mastodon notifications: middle click to view"
                          'local-map (make-mode-line-mouse-map
                                      'mouse-2 'mastodon-notifications--get))))
    (when
        ;; buffer not yet killed:
        (and (buffer-live-p buffer)
             ;; buffer is a mastodon buffer:
             (mastodon-notifications--masto-buffer-p (current-buffer)))
      (setq mastodon-notifications-modeline-indicator notifs-display)
      ;; if not a member of `mode-line-misc-info', add it, once only:
      (unless (assoc 'mastodon-tl--buffer-spec mode-line-misc-info)
        (add-to-list 'mode-line-misc-info
                     ;; variable that must be non-nil if notifs are to be
                     ;; displayed:
                     `(mastodon-tl--buffer-spec
                       ;; display string:
                       ,mastodon-notifications-modeline-indicator)))
      (when (and mastodon-notifications-reload-when-new
                 (equal (buffer-name buffer) "*mastodon-notifications*")
                 (> count 0))
        (mastodon-notifications--get)))))

;; FIXME: when can we possibly cleanup notifs timer?
;; notifs check cd be a minor mode, enabled when the var is t?
;; (add-hook 'change-major-mode-hook #'mastodon-notifications--cleanup-timer)
(defun mastodon-notifications--cleanup-timer ()
  "Cancel the modeline notifications timer.
Also restore `mode-line-misc-info' to its previous value."
  (cancel-timer mastodon-notifications-new-notifications-timer)
  ;; this isn't necessary as we now only set it when in a masto buffer:
  (setq mode-line-misc-info
        (delete
         (assoc 'mastodon-tl--buffer-spec mode-line-misc-info)
         mode-line-misc-info)))



(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here

