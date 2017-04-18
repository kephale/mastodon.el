;;; mastodon-toot.el --- Minor mode for sending Mastodon toots

;; Copyright (C) 2017 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Homepage: https://github.com/jdenen/mastodon.el

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

;; mastodon-toot.el supports POSTing status data to Mastodon.

;;; Code:

(require 'mastodon-auth)
(require 'mastodon-http)

(defgroup mastodon-toot nil
  "Capture Mastodon toots."
  :prefix "mastodon-toot-"
  :group 'mastodon)

(defun mastodon-toot--send-triage (status)
  "Callback function to triage toot POST.

STATUS is passed by `url-retrieve'."
  (mastodon--http-response-triage status
                                  (lambda () (switch-to-buffer (current-buffer))))) ;; FIXME

(defun mastodon-toot-send ()
  "Kill new-toot buffer/window and POST contents to the Mastodon instance."
  (interactive)
  (let ((toot (buffer-string))
        (endpoint (mastodon--api-for "statuses")))
    (progn
      (kill-buffer-and-window)
      (mastodon--http-post endpoint
                           'mastodon-toot--send-triage
                           `(("status" . ,toot))
                           `(("Authorization" . ,(concat
                                                  "Bearer "
                                                  (mastodon--access-token))))))))

(defun mastodon-toot-cancel ()
  "Kill new-toot buffer/window. Does not POST content to Mastodon."
  (interactive)
  (kill-buffer-and-window))

(defun mastodon-toot--action-success (marker)
  "Insert MARKER with 'success face in byline."
  (let ((inhibit-read-only t))
    (mastodon-tl--property 'toot-id)
    (goto-char (+ 3 (point)))
    (insert (format "(%s) "
                    (propertize marker
                                'face 'success)))))

(defun mastodon-toot--action-triage (response callback)
  "Parse response code from RESPONSE buffer.

Execute CALLBACK function if response was OK."
  (let ((status (with-current-buffer response
                  (mastodon--response-code))))
    (if (string-prefix-p "2" status)
        (funcall callback)
      (switch-to-buffer response))))

(defun mastodon-toot--action (action callback)
  "Take action on toot at point."
  (let* ((id (mastodon-tl--property 'toot-id))
         (url (mastodon--api-for (concat "statuses/"
                                         (number-to-string id)
                                         "/"
                                         action))))
    (let ((response (mastodon-http--post url nil nil)))
      (mastodon-toot--action-triage response callback))))

(defun mastodon-toot--boost ()
  "Boost toot at `point'."
  (interactive)
  (let ((callback (lambda () (mastodon-toot--action-success "B"))))
    (mastodon-toot--action "reblog" callback)))

(defun mastodon-toot--favourite ()
  "Favourite toot at `point'."
  (interactive)
  (let ((callback (lambda () (mastodon-toot--action-success "F"))))
    (mastodon-toot--action "favourite" callback)))

(defvar mastodon-toot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mastodon-toot-send)
    (define-key map (kbd "C-c C-k") #'mastodon-toot-cancel)
      map)
  "Keymap for `mastodon-toot-mode'.")

(define-minor-mode mastodon-toot-mode
  "Minor mode to capture Mastodon toots."
  :group 'mastodon-toot
  :keymap mastodon-toot-mode-map
  :global nil)

(provide 'mastodon-toot)
;;; mastodon-toot.el ends here
