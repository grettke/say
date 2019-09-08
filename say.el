;;; say.el --- Send text to system `say' command.     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wisdom and Wonder

;; Author: Grant Rettke <grant@wisdomandonder.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun say--line (&optional line)
  "Say current line."
  (interactive)
  (setq line (or line (thing-at-point 'line)))
  (start-process "say" nil "say" line))

(defun say--next-line ()
  "Say next line."
  (interactive)
  (next-line)
  (say--line))

(defun say--previous-line ()
  "Say previous line."
  (interactive)
  (previous-line)
  (say--line))

;;;###autoload
(define-minor-mode say-mode
  "Toggle Say mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

Say mode asynchronously sends the current line of text to the
system `say' command.

When enabled Say mode makes the buffer read-only by enabling
`read-only-mode'. When disabled Say mode makes the buffer read-write by
disabling `read-only-mode'.

TODO: Handle starting in read only mode

Say mode lets you operate it using one hand on either the left or
right side of the keyboard. It uses familiar VI and Emacs
navigation bindings. The bindings are mirrored across the center
of the keyboard in an attempt to retain a sort of muscle memory
between both hands. This is useful for example you write your
presentation's content in Org and are developing a screencast to
present along with it. Say mode frees you up from having to
constantly look back and forth between them. After working on the
same screencast for hours and hours any way to speed up the
process help.

When Say mode is enabled this is the keymap for the right side:

- l: say the current line.
- j: move the cursor down one line and say the line.
- n: move the cursor down one line.
- k: move the cursor up one line and say the line.
- p: move the cursor up one line.
- u: toggle the mode.

When Say mode is enabled this is the keymap for the left side:

- s: say the current line.
- f: move the cursor down one line and say the line.
- v: move the cursor down one line.
- d: move the cursor up one line and say the line.
- q: move the cursor up one line.
- t: toggle the mode.

Sometimes Say mode doesn't say what you expect or anything at all. When this
hapens try to use your system's `say' command using the line of text on which
you are operating. If that doesn't work then figure out why it isn't doing
what you expect. If it works there but not in Say mode then please fill out a
help request.
"
  :lighter " Say"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "l") #'say--line)
            (define-key map (kbd "j") #'say--next-line)
            (define-key map (kbd "n") #'next-line)
            (define-key map (kbd "k") #'say--previous-line)
            (define-key map (kbd "p") #'previous-line)
            (define-key map (kbd "u") #'say-mode)
            (define-key map (kbd "s") #'say--line)
            (define-key map (kbd "f") #'say--next-line)
            (define-key map (kbd "v") #'next-line)
            (define-key map (kbd "d") #'say--previous-line)
            (define-key map (kbd "q") #'previous-line)
            (define-key map (kbd "t") #'say-mode)
            map)
  (read-only-mode (if (bound-and-true-p say-mode) 1 0)))

(provide 'say)
;;; say.el ends here
