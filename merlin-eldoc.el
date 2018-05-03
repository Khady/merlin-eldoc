;;; merlin-eldoc.el --- eldoc for OCaml and Reason  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Louis Roché

;; Author: Louis Roché <louis@louisroche.net>
;; Created: 27 April 2018
;; Version: 1.1
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

;; TODO:
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

(defcustom merlin-eldoc-skip-on-merlin-error t
  "Don't show anything if merlin marked the area where the point is as an error.
If nil it is possible that eldoc and merlin will fight to show
information and error at the same time.  Only one tool can win."
  :type 'boolean
  :group 'merlin-eldoc)

(defcustom merlin-eldoc-max-lines
  (cond ((equal eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit) 8)
        ((equal eldoc-echo-area-use-multiline-p t) 8)
        ((equal eldoc-echo-area-use-multiline-p nil) 1))
  "Maximum number of lines that will be used to display information.

The final result depends on `eldoc-echo-area-use-multiline-p'.
If value is nil, this setting will be ignored and result will be
a single line.  If value is truncate-sym-name-if-fit, result will
be as compact as possible.  For example if type and documentation
are both one line long and can be combined to fit on a single
line, they will be merged into one line."
  :type 'integer
  :group 'merlin-eldoc)

(defcustom merlin-eldoc-max-lines-type merlin-eldoc-max-lines
  "Maximum number of lines the type can use.

If value is more than `merlin-eldoc-max-lines', it is replaced by
`merlin-eldoc-max-lines'.  If less than 1, it is replaced by 1.

See documentation of `merlin-eldoc-max-lines-doc' for more
details on interaction between `merlin-eldoc-max-lines-type' and
`merlin-eldoc-max-lines-doc'"
  :type 'integer
  :group 'merlin-eldoc)

(defcustom merlin-eldoc-max-lines-doc merlin-eldoc-max-lines
  "Maximum number of lines the documentation can use.

If value is fit, the documentation is wrapped into one line and
then truncated.  It is displayed a line with the type.

If value is single, the same behavior is applied except that
documentation will use a dedicated line.

If documentation contains more lines and can't be wrapped into
one line, it will be truncated to the maximum number of lines
allowed.

When both type and documentation are to be displayed, type takes
priority over documentation.

For example with the given configuration:

  - `merlin-eldoc-max-lines': 5
  - `merlin-eldoc-max-lines-doc': 4
  - `merlin-eldoc-max-lines-type': 4

If the type is 3 lines long, only 2 lines will be available for
the documentation."
  :type '(radio (integer :tag "number of lines'")
                (const :tag "dedicate a single line to the doc" single)
                (const :tag "fit the type and doc on one line" fit))
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
  (let ((symbol (thing-at-point 'symbol))
        (string (merlin-eldoc--in-string-p pos))
        (comment (merlin-eldoc--in-comment-p pos))
        (keyword (merlin-eldoc--is-keyword-p pos)))
    (and (or symbol string)
         (not comment)
         (or (not keyword) string))))

(defun merlin-eldoc--multiline-p (s)
  "Check if there are multiple lines in S."
  (if (string-match "\n" s) t nil))

(defun merlin-eldoc--minibuffer-width ()
  "Get writable width of the minibuffer."
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

(defvar merlin-eldoc--doc-error-messages
  '("No documentation available"
    "Not a valid identifier"
    "Not in environment '*'"
    "is a builtin, no documentation is available")
  "List of invalid values for the documentation.")

(defun merlin-eldoc--skip-doc-p (doc)
  "Look for invalid values of DOC based on value `merlin-eldoc--doc-error-messages'."
  (find-if (lambda (s) (string-match-p s doc))
           merlin-eldoc--doc-error-messages))

(defun merlin-eldoc--merlin-error-at-point-p ()
  "Search if place under point is marked as error by merlin."
  (when merlin-mode
    (let* ((pos (point)) (beg pos) (end (1+ pos))
           (errors (overlays-in beg end)))
      (find-if 'merlin--overlay-pending-error errors))))

(defun merlin-eldoc--count-lines (s)
  "Count number of lines in string S."
  (let ((count 1) (pos 0))
    (while (string-match "\n" s pos)
      (setq pos (match-end 0))
      (setq count (1+ count)))
    count))

(defun merlin-eldoc--short-p (s max)
  "Check if S contains less than MAX lines."
  (<= (merlin-eldoc--count-lines s) max))

(defun merlin-eldoc--wrap (text)
  "Trim all lines of TEXT and merge them in one line."
  (string-join (delete "" (split-string text)) " "))

;;; Main logic

(defvar merlin-eldoc--max-lines merlin-eldoc-max-lines
  "Local copy of `merlin-eldoc-max-lines' adjusted to follow eldoc config.")

(defvar merlin-eldoc--max-lines-type merlin-eldoc-max-lines-type
  "Local copy of `merlin-eldoc-max-lines-type' adjusted to follow max lines.")

(defvar merlin-eldoc--max-lines-doc merlin-eldoc-max-lines-doc
  "Local copy of `merlin-eldoc-max-lines' adjusted to follow max lines.")

(defun merlin-eldoc--adjust-max-len ()
  "Adjust max len variables based on eldoc settings."
  (when (not eldoc-echo-area-use-multiline-p)
    (setq-local merlin-eldoc--max-lines 1))
  (when (> merlin-eldoc--max-lines-type merlin-eldoc--max-lines)
    (setq-local merlin-eldoc--max-lines-type merlin-eldoc--max-lines))
  (when (and (integerp merlin-eldoc--max-lines-doc)
             (> merlin-eldoc--max-lines-doc merlin-eldoc--max-lines))
    (setq-local merlin-eldoc--max-lines-doc merlin-eldoc--max-lines)))

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
                   (merlin-eldoc--short-p (merlin--type-enclosing-text data)
                                          merlin-eldoc--max-lines))
              (merlin--type-enclosing-text data)
            (concat "(* type is too long, check buffer `"
                    merlin-type-buffer-name
                    "' or query the type manually *)")))))))

(defun merlin-eldoc--raw-doc ()
  "Gather raw documentation of the thing at point."
  (if (and merlin-eldoc-doc (not (merlin-eldoc--in-string-p (point))))
      (let ((doc (merlin--document-pos nil)))
        (if (not (merlin-eldoc--skip-doc-p doc))
            (string-trim doc)))))

(defun merlin-eldoc--shape (doc &optional type-lines)
  "Find display shape of DOC based on TYPE-LINES and eldoc config.
DOC must be a list of lines and TYPE-LINES a count of lines in type.
The value returned is one of:
  - fit (doc and type should fit on a line),
  - single (doc should fit on one dedicated line)
  - multi (doc should fit in multiple lines)."
  (let* ((doc-lines (length doc))
         (type-lines (if type-lines type-lines 0))
         (doc-max-lines (min (- merlin-eldoc--max-lines type-lines)
                             merlin-eldoc--max-lines-doc))
         (doc-type (cond ((integerp merlin-eldoc--max-lines-doc) 'multi)
                         (t merlin-eldoc--max-lines-doc))))
    (cond ((not eldoc-echo-area-use-multiline-p) 'fit)
          ((> type-lines 1)
           (cond ((> doc-max-lines 1) doc-type)
                 ((= doc-max-lines 1) 'single)
                 ((= doc-max-lines 0) 'fit)))
          ((or (equal eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
               (equal doc-type 'fit))
           'fit)
          (eldoc-echo-area-use-multiline-p doc-type))))

(defun merlin-eldoc--format-doc-multi (doc type-lines)
  "Format DOC to fit on multiple lines leaving space for TYPE-LINES."
  (let* ((doc-lines (length doc))
         (doc-max-lines (min (- merlin-eldoc--max-lines type-lines)
                             merlin-eldoc--max-lines-doc)))
    (if (> doc-lines doc-max-lines)
        (concat
         (string-join (butlast doc (- doc-lines doc-max-lines)) "\n")
         merlin-eldoc-truncate-marker)
      (string-join doc "\n"))))

(defun merlin-eldoc--format-doc-single (doc)
  "Format DOC to fit on a single line."
  (let* ((com-len (+ (length comment-start) (length comment-end)))
         (doc (merlin-eldoc--wrap doc))
         (max-width (- (merlin-eldoc--minibuffer-width) com-len))
         (max-trunc (- max-width (length merlin-eldoc-truncate-marker))))
    (if (> (length doc) max-width)
        (concat (substring doc 0 max-trunc) merlin-eldoc-truncate-marker)
      doc)))

(defun merlin-eldoc--format-doc-fit (doc type)
  "Format DOC to fit with TYPE on a single line."
  (let* ((com-len (+ (length comment-start) (length comment-end)))
         (doc (merlin-eldoc--wrap doc))
         (type-len (if type (length type) 0))
         (delimiter-len (if type (length merlin-eldoc-delimiter) 0))
         (max-width (- (merlin-eldoc--minibuffer-width) com-len type-len delimiter-len))
         (max-trunc (- max-width (length merlin-eldoc-truncate-marker))))
    (cond ((and (> (length doc) max-width) (>= max-width 10))
           (concat (substring doc 0 max-trunc) merlin-eldoc-truncate-marker))
          ((< max-width 10) nil)
          (t doc))))

(defun merlin-eldoc--format-doc (doc &optional type)
  "Format DOC for display in echo area.  Adapt to TYPE if provided."
  (let* ((doc-split (delete "" (split-string doc "[\r\n]+")))
         (type-lines (if type (merlin-eldoc--count-lines type) 0))
         (shape (merlin-eldoc--shape doc-split type-lines))
         (doc (cond
               ((equal shape 'multi) (merlin-eldoc--format-doc-multi doc-split type-lines))
               ((equal shape 'single) (merlin-eldoc--format-doc-single doc))
               ((equal shape 'fit) (merlin-eldoc--format-doc-fit doc type))))
         (doc (if doc (concat comment-start doc comment-end))))
    (cond ((not type) doc)
          ((equal shape 'fit) (concat type merlin-eldoc-delimiter doc))
          (t (concat doc "\n" type)))))

(defun merlin-eldoc--gather-info ()
  "Get information about the thing at point and format them into a string."
  (interactive)
  (when (and (not (string-equal merlin-type-buffer-name (buffer-name)))
             (not (minibufferp))
             (merlin-eldoc--valid-position-p (point))
             (not (and merlin-eldoc-skip-on-merlin-error
                       (merlin-eldoc--merlin-error-at-point-p))))
    (merlin-eldoc--adjust-max-len)
    (let* ((type (merlin-eldoc--type))
           (doc (merlin-eldoc--raw-doc))
           (output (cond ((and type doc) (merlin-eldoc--format-doc doc type))
                         (type type)
                         (doc (merlin-eldoc--format-doc doc))
                         (t nil))))
      (if output (merlin-eldoc--fontify output)))))

;;;###autoload
(defun merlin-eldoc-setup ()
  "Setup eldoc for OCaml/Reasonml based on merlin."
  (interactive)
  (setq-local eldoc-documentation-function #'merlin-eldoc--gather-info)
  (eldoc-mode t))

;;;###autoload
(defun merlin-eldoc-customize ()
  "Open the customize buffer for the group merlin-eldoc."
  (interactive)
  (customize-group 'merlin-eldoc))

(provide 'merlin-eldoc)

;;; merlin-eldoc.el ends here
