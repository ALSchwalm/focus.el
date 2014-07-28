;;; focus.el --- A mode to help focus while coding in GNU Emacs

;; Copyright (c) 2014 Adam Schwalm
;; Author: Adam Schwalm <adamschwalm@gmail.com>
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use 'focus-focus-buffer' to focus on the current buffer, and
;; focus-unfocus-buffer to return to the previous window
;; configuration.

;; 'focus-toggle-focus' will toggle between a focused and unfocused state.

;;; Code:

(defvar focus-saved-buffer nil)
(defvar focus-pre-focus-config nil)
(defvar focus-buffer-width 85)
(defvar focus-space-buffer-name "*spacing*")

(defun focus-focus-buffer ()
  "Focus on the current buffer."
  (interactive)
  (setq focus-pre-focus-config (current-window-configuration))
  (setq focus-saved-buffer (current-buffer))
  (delete-other-windows)
  (get-buffer-create focus-space-buffer-name)
  (switch-to-buffer focus-space-buffer-name)
  (let ((spacing-width))
    (setq spacing-width (/ (- (frame-width) focus-buffer-width) 2))
    (split-window-right spacing-width)
    (other-window 1)
    (split-window-right focus-buffer-width)
    (setq mode-line-format nil)
    (switch-to-buffer focus-saved-buffer)))

(defun focus-unfocus-buffer ()
  "Return to the window configuration before 'focus-focus-buffer' was invoked."
  (interactive)
  (set-window-configuration focus-pre-focus-config)
  (kill-buffer focus-space-buffer-name))

(defun focus-toggle-focus ()
  "Toggle between a focus and unfocused state."
  (interactive)
  (if (get-buffer focus-space-buffer-name)
      (focus-unfocus-buffer)
    (focus-focus-buffer)))

(provide 'focus)
;;; focus.el ends here
