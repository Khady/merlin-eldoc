;;; merlin-eldoc.el --- eldoc for OCaml and Reason  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Louis Roché

;; Author: Louis Roché <louis@louisroche.net>
;; Created: 27 April 2018
;; Version: 1.0
;; Keywords: merlin ocaml languages eldoc
;; Homepage: https://github.com/khady/merlin-eldoc
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

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
;; merlin-eldoc is a wrapper of the Emacs merlin mode.  It enables
;; automatic display of information available in merlin for OCaml
;; code.

;; Installation:
;; You need merlin-mode amd merlin installed on your
;; system (ocamlmerlin binary) for merlin-eldoc to work.

;;; Code:

(require 'eldoc)
(require 'newcomment)
(require 'merlin)

(defgroup merlin-eldoc nil
  "Eldoc for OCaml/Reasonml based on merlin."
  :group 'merlin
  :prefix "merlin-eldoc-")

(defcustom merlin-eldoc-type t
  "Enable display of type for the thing at point."
  :type 'boolean
  :group 'merlin-eldoc)

(defcustom merlin-eldoc-doc t
  "Enable display of documentation for the thing at point."
  :type 'boolean
  :group 'merlin-eldoc)

(defcustom merlin-eldoc-occurrences t
  "Enable highlight of other occurrences of the thing at point."
  :type 'boolean
  :group 'merlin-eldoc)

(defcustom merlin-eldoc-delimiter "     "
  "Delimiter between type and documentation if both are to be displayed."
  :type 'string
  :group 'merlin-eldoc)

(defcustom merlin-eldoc-truncate-marker "..."
  "Marker used to show when the documentation has been truncated."
  :type 'string
  :group 'merlin-eldoc)

;;; Utils

;; imported from evil-matchit
(defun merlin-eldoc--current-font-among-fonts-p (pos fonts)
  "If current font at POS is among FONTS."
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar (lambda (f)
                    (member f fonts))
                  fontfaces))))

(defun merlin-eldoc--in-comment-p (pos)
  "Check character at POS is comment or documentation by comparing font face."
  (merlin-eldoc--current-font-among-fonts-p pos '(font-lock-comment-face
                                                  font-lock-comment-delimiter-face
                                                  font-lock-doc-face)))


(defun merlin-eldoc--in-string-p (pos)
  "Check character at POS is string by comparing font face."
  (merlin-eldoc--current-font-among-fonts-p pos '(font-lock-string-face)))

(defun merlin-eldoc--is-keyword-p (pos)
  "Check if character at POS is keyword by comparing font face."
  (merlin-eldoc--current-font-among-fonts-p pos '(tuareg-font-lock-governing-face
                                                    font-lock-keyword-face)))

(defun merlin-eldoc--valid-position-p (pos)
  "Check if POS is in a place valid to get a type."
  (and (not (merlin-eldoc--in-comment-p pos))
       (or (not (merlin-eldoc--is-keyword-p pos)) (merlin-eldoc--in-string-p pos))))

(defun merlin-eldoc--multiline-p (s)
  "Check if there are multiple lines in S."
  (if (string-match "\n" s) t nil))

(defun merlin-eldoc--ea-width ()
  "Get writable width of the echo area."
  ;; Subtract 1 from window width since emacs will not write
  ;; any chars to the last column, or in later versions, will
  ;; cause a wraparound and resize of the echo area.
  (1- (window-width (minibuffer-window))))

(defun merlin-eldoc--fontify (s)
  "Fontify the string S."
  (if (not s) "<no information>")
  (merlin/display-in-type-buffer s)
  (with-current-buffer merlin-type-buffer-name
    (font-lock-fontify-region (point-min) (point-max))
    (buffer-string)))

;;; Main logic

(defun merlin-eldoc--type ()
  "Gather type of the symbol at point."
  (if (not merlin-eldoc-type)
      nil
    (setq merlin--verbosity-cache nil) ; reset verbosity to not display deeper types
    (if (region-active-p)
        (merlin--type-region)
      (merlin--type-enclosing-query)
      (when merlin-enclosing-types
        (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
          (if (and (cddr data)
                   (merlin--is-short (merlin--type-enclosing-text data)))
              (merlin--type-enclosing-text data)
            (concat "(* type is too long, check buffer `"
                    merlin-type-buffer-name
                    "' or query the type manually *)")))))))

(defun merlin-eldoc--raw-doc ()
  "Gather raw documentation of the thing at point."
  (if merlin-eldoc-doc (merlin--document-pos nil)))

(defun merlin-eldoc--max-doc-length (type delim)
  "Compute the maximum length allowed for the documentation base on TYPE and DELIM."
  (cond ((merlin-eldoc--multiline-p type) (merlin-eldoc--ea-width))
        (t (- (merlin-eldoc--ea-width)
              (length type)
              (length merlin-eldoc-delimiter)))))

(defun merlin-eldoc--wrap-doc (doc)
  "Trim all lines of DOC and merge them in one line"
  (string-join (delete "" (split-string doc)) " "))

(defun merlin-eldoc--format-doc (raw-doc &optional max)
  "Format documentation for display in echo area.
Wrap RAW-DOC to a single line if total length is lte MAX.
Otherwise take only the first line.  Add comment delimiters.
Return nil if the doc doesn't fit"
  (let* ((raw-doc (string-trim raw-doc))
         (nl (string-match "\n" raw-doc))
         (raw-doc (merlin-eldoc--wrap-doc raw-doc))
         (com-len (+ (length comment-start) (length comment-end)))
         (max (- (if max max (merlin-eldoc--ea-width)) com-len)) ; take into account the comment delimiters
         (short (<= (length raw-doc) max))
         (max-trunc (- max (length merlin-eldoc-truncate-marker)))
         (doc
          (cond (short
                 ;; The doc fits on one line.
                 raw-doc)
                ((and nl (<= nl max-trunc))
                 ;; Display the first line.  Try to display the
                 ;; truncate marker if there is space after the first
                 ;; newline.
                 (concat (substring raw-doc 0 nl) merlin-eldoc-truncate-marker))
                ((and nl (<= nl max))
                 ;; Display the first line even the there is no space
                 ;; for the truncate marker.
                 (substring raw-doc 0 nl))
                ((and (> max-trunc 10))
                 ;; The doc is too long to be nicely formated, but
                 ;; there is enough space to display relevant
                 ;; information. Return a truncated version.
                 (concat (substring raw-doc 0 max-trunc) merlin-eldoc-truncate-marker))
                (t nil))))
    (if doc (concat comment-start doc comment-end))))

(defun merlin-eldoc--eldoc ()
  "Get information about the thing at point for `eldoc-mode'."
  (interactive)
  (when (and (thing-at-point 'symbol)
             (not (string-equal merlin-type-buffer-name (buffer-name)))
             (not (minibufferp))
             (merlin-eldoc--valid-position-p (point)))
    (let* ((type (merlin-eldoc--type))
           (doc (merlin-eldoc--raw-doc))
           (occurrences nil))
      (merlin-eldoc--fontify
       (cond ((and type doc)
              (let* ((max-doc-len (merlin-eldoc--max-doc-length type merlin-eldoc-delimiter))
                     (formated-doc (merlin-eldoc--format-doc doc max-doc-len))
                     (multiline-type (merlin-eldoc--multiline-p type)))
                (cond ((and formated-doc multiline-type) (concat formated-doc "\n" type))
                      (formated-doc (concat type merlin-eldoc-delimiter formated-doc))
                      (t type))))
             (type type)
             (doc doc))))))

;;;###autoload
(defun merlin-eldoc-setup ()
  "Setup eldoc for OCaml/Reasonml based on merlin."
  (interactive)
  (setq-local eldoc-documentation-function #'merlin-eldoc--eldoc)
  (eldoc-mode t))

(provide 'merlin-eldoc)

;;; merlin-eldoc.el ends here
