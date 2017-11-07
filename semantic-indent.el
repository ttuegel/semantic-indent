;;; semantic-indent.el --- Minor mode for semantically-significant indentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Thomas Tuegel

;; Author: Thomas Tuegel <ttuegel@mailbox.org>
;; Keywords: convenience, files, languages

;; This program is free software; you can redistribute it and/or modify
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

;; Attempts to automatically indent text where indentation is semantically
;; significant generally destroy the semantics. `semantic-indent-mode' is
;; a minor mode to work with indentation without destroying the semantics
;; intended by the author.

;;; Code:

(defvar semantic-indent-shift-width 4
  "The default unit of indentation.")

(defun semantic-indent/indent-line-to (column)
  (let ((indent-tabs-mode nil)) (indent-line-to column)))

(defun semantic-indent/shift-line (width)
  "Shift the current line by WIDTH.
Leave point at the end of indentation."
  (back-to-indentation)
  (semantic-indent/indent-line-to (max 0 (+ (current-column) width))))

(defun semantic-indent/shift-right-line (&optional half-width)
  (interactive "P")
  (semantic-indent/shift-line
   (/ semantic-indent-shift-width (if half-width 2 1))))

(defun semantic-indent/shift-left-line (&optional half-width)
  (interactive "P")
  (let* ((width (/ (- semantic-indent-shift-width) (if half-width 2 1)))
         (cur-indent (current-indentation))
         (min-indent (- cur-indent width)))
    (semantic-indent/indent-line-to
     (semantic-indent/prev-line-indent cur-indent min-indent))))

(defun semantic-indent/backward-line-for-indent ()
  (let (done)
    (setq done (= 0 (forward-line -1)))
    (when (semantic-indent/is-empty-line) (setq done (= 0 (forward-line -1))))
    done))

(defun semantic-indent/prev-indent (max-indent &optional min-indent)
  "Search backward for an indent point between MIN-INDENT and MAX-INDENT.

MIN-INDENT defaults to 0 if not set."
  (when (not min-indent) (setq min-indent 0))
  (save-excursion
    (let* ((ind (current-indentation)))
      (while (and (>= ind max-indent) (>= ind min-indent))
        (semantic-indent/backward-line-for-indent)
        (setq ind (current-indentation)))
      (max ind min-indent))))

(defun semantic-indent/is-blank-line ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[[:space:]]*$")))

(defun semantic-indent/delete-line ()
  (delete-region (line-beginning-position) (line-end-position)))

(defun semantic-indent/current-indentation (&optional n)
  (save-excursion
    (forward-line (or n 0))
    (current-indentation)))

(defun semantic-indent/is-empty-line ()
  (= (line-beginning-position) (line-end-position)))

(defun semantic-indent/round-indent-up (ind)
  (let ((r (% ind semantic-indent-shift-width))
        (q (/ ind semantic-indent-shift-width)))
    (* (if (= 0 r) q (1+ q)) semantic-indent-shift-width)))

(defun semantic-indent/default-indent ()
  (save-excursion
    (semantic-indent/backward-line-for-indent)
    (semantic-indent/round-indent-up (current-indentation))))

(defun semantic-indent/newline-and-indent ()
  (interactive)
  (when (semantic-indent/is-blank-line) (semantic-indent/delete-line))
  (let ((actual-indentation (semantic-indent/default-indent)))
    (newline)
    (semantic-indent/indent-line-to actual-indentation)))

(define-minor-mode semantic-indent-mode
  "A minor-mode for semantically-significant indentation."
  nil  ; init-value
  nil  ; lighter
  (list  ; keymap
   (cons (kbd "<return>")  #'semantic-indent/newline-and-indent)
   (cons (kbd "<tab>")     #'semantic-indent/shift-right-line)
   (cons (kbd "<backtab>") #'semantic-indent/shift-left-line))
  (progn  ; body
    (setq-local semantic-indent/electrify electric-indent-mode)
    (if semantic-indent-mode
        (electric-indent-local-mode -1)
      (electric-indent-local-mode semantic-indent/electrify))))

(provide 'semantic-indent)
;;; semantic-indent.el ends here
