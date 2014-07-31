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
(defvar focus-space-buffer-name " *spacing*")
(defvar focus-vertical-border-color nil)
(defvar focus-mode-line-format nil)
(defvar focus-focused nil)

(defun focus-focus-buffer (&optional show-mode-line)
  "Focus on the current buffer.
If SHOW-MODE-LINE is non-nil, show the mode line of the focused buffer."
  (interactive)
  (setq focus-focused t)
  (setq focus-pre-focus-config (current-window-configuration))
  (setq focus-saved-buffer (current-buffer))
  (delete-other-windows)
  (switch-to-buffer focus-space-buffer-name)
  (let ((spacing-width))
    (setq spacing-width (/ (- (frame-width) focus-buffer-width) 2))
    (split-window-right spacing-width)
    (other-window 1)
    (split-window-right focus-buffer-width)
    (setq mode-line-format nil)
    (unless focus-vertical-border-color
      (setq focus-vertical-border-color (face-attribute 'vertical-border :foreground)))
    (set-face-attribute 'vertical-border nil
                        :foreground (face-attribute 'default :background))
    (switch-to-buffer focus-saved-buffer)
    (unless show-mode-line
     (unless focus-mode-line-format
       (setq focus-mode-line-format mode-line-format))
     (setq mode-line-format nil)))
  (add-hook 'window-configuration-change-hook 'focus-config-hook))

(defun focus-unfocus-buffer ()
  "Return to the window configuration before 'focus-focus-buffer' was invoked."
  (interactive)
  (setq focus-focused nil)
  (remove-hook 'window-configuration-change-hook 'focus-config-hook)
  (set-window-configuration focus-pre-focus-config)
  (set-face-attribute 'vertical-border nil
                      :foreground focus-vertical-border-color)
  (when focus-mode-line-format
    (setq mode-line-format focus-mode-line-format))
  (kill-buffer focus-space-buffer-name))

(defun focus-config-hook ()
  "Function run when configuration changed while focused."
  (remove-hook 'window-configuration-change-hook 'focus-config-hook)
  (when focus-mode-line-format
    (setq mode-line-format focus-mode-line-format))
  (when focus-vertical-border-color
    (set-face-attribute 'vertical-border nil
                        :foreground focus-vertical-border-color)))


(defun focus-toggle-focus ()
  "Toggle between a focus and unfocused state."
  (interactive)
  (if (get-buffer-window focus-space-buffer-name)
      (focus-unfocus-buffer)
    (focus-focus-buffer)))

(defun focus-widen-focus (&optional amount)
  "Increase the size of the focus buffer by AMOUNT characters on each side.
AMOUNT defaults to 5 if not given."
  (interactive)
  (setq focus-buffer-width (+ (or amount 5) focus-buffer-width))
  (focus-toggle-focus)
  (focus-toggle-focus))

(defun focus-narrow-focus (&optional amount)
  "Decrease the size of the focus buffer by AMOUNT characters on each side.
AMOUNT defaults to 5 if not given."
  (interactive)
  (focus-widen-focus (- (or amount 5))))

(provide 'focus)
;;; focus.el ends here
