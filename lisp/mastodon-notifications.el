;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Maintainer: Marty Hiatt <martianhiatus@riseup.net>
;; Version: 0.10.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://git.blast.noho.st/mouse/mastodon.el

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

(require 'persist)

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
(defvar mastodon-tl--display-media-p)
(defvar mastodon-tl--buffer-spec)

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

(persist-defvar mastodon-notifications-newest-id nil
  "The ID of the newest notification already loaded and seen
  locally. It's value is saved between sessions by `persist'.")

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
  (when (mastodon-tl--find-property-range 'toot-json (point))
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
                                           (unless f-reqs-view-p
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
  (mastodon-notifications--format-note note 'favorite))

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
     (if (or (equal type 'follow)
             (equal type 'follow-request))
         ;; Using reblog with an empty id will mark this as something
         ;; non-boostable/non-favable.
         (cons '(reblog (id . nil)) note)
       status)
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
              ((equal type 'favorite)
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
     id)))

(defun mastodon-notifications--insert-status (toot body author-byline action-byline id)
  "Display the content and byline of timeline element TOOT.

BODY will form the section of the toot above the byline.

AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'.

ACTION-BYLINE is also an optional function for adding an action,
such as boosting favouriting and following to the byline. It also
takes a single function. By default it is
`mastodon-tl--byline-boosted'.

ID is the notification's own id, which is attached as a property."
  (mastodon-tl--insert-status toot body author-byline action-byline id))

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
  (mapc #'mastodon-notifications--by-type json)
  (goto-char (point-min))
  ;;set newest ID for notifications modeline alerts:
  (setq mastodon-notifications-newest-id
        (mastodon-tl--property 'toot-id)))

(defun mastodon-notifications--get ()
  "Display NOTIFICATIONS in buffer."
  (interactive)
  (message "Loading your notifications...")
  (mastodon-tl--init
   "notifications"
   "notifications"
   'mastodon-notifications--timeline))

(defun mastodon-notifications--check-for-new-timer ()
  "Run `mastodon-notifications--check-for-new' via a timer.
Easy coz no args hassle."
  (mastodon-notifications--check-for-new mastodon-notifications-newest-id))

(defun mastodon-notifications--set-and-run-timer ()
  "Run a timer to check for new notifications.
First we cancel any existing timers to avoid them accumulating.
Run in `mastodon-mode-hook'."
  (when mastodon-notifications-new-notifications-timer
    (cancel-timer mastodon-notifications-new-notifications-timer))
  (setq mastodon-notifications-new-notifications-timer
        (run-at-time nil 5 #'mastodon-notifications--check-for-new-timer)))

(defun mastodon-notifications--check-for-new (newest-id)
  "Check the server for new notifications since NEWEST-ID.
Runs `mastodon-notifications--modeline-display-unread-count' on the response."
  ;;only run in masto mode:
  (when (or (equal major-mode 'mastodon-mode)
            (string= "*new toot*" (buffer-name)))
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
         (count-display (propertize
                         (number-to-string count)
                         'face 'mastodon-boosted-face))
         (notifs-display (propertize
                          (if (require 'all-the-icons nil :no-error)
                              ""
                            "notifs:")
                          'face 'mastodon-display-name-face))
         (display (propertize (concat notifs-display count-display)
                              'mouse-face 'mode-line-highlight
                              'help-echo "mastodon notifications count")))
    (when (and
           ;; mastodon buffer:
           (let ((name (buffer-name buffer)))
             (or (string-prefix-p "*mastodon" name)
                 (string= "*new toot*" name)))
           ;; buffer not yet killed:
           (buffer-live-p buffer))
      (with-current-buffer buffer
        ;; this check prevents reset to 0
        ;; better solution to displaying 0 is to remove it entirely
        ;; (if (> count 0)
        (setq-local mode-line-misc-info display))
      ;; reload if in notifs view and we have new notifs:
      (when (and (equal (buffer-name buffer) "*mastodon-notifications*")
                 (> count 0))
        (mastodon-notifications--get)))))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
