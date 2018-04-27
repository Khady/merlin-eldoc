(require 'merlin)

;; imported from evil-matchit
(defun merlin--hl-current-font-among-fonts-p (pos fonts)
  "If current font at POS is among FONTS."
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar (lambda (f)
                    (member f fonts))
                  fontfaces))))

(defun merlin--hl-in-comment-p (pos)
  "Check character at POS is comment by comparing font face."
  (merlin--hl-current-font-among-fonts-p pos '(font-lock-comment-face
                                               font-lock-comment-delimiter-face)))


(defun merlin--hl-in-string-or-doc-p (pos)
  "Check character at POS is string or document by comparing font face."
  (merlin--hl-current-font-among-fonts-p pos '(font-lock-string-face
                                               font-lock-doc-face)))

(defcustom merlin-hl-identifier-idle-time 0.50
  "How long to wait after user input before highlighting the current identifier."
  :type 'float
  :group 'merlin)

(defvar merlin--hl-identifier-timer
  nil
  "The global timer used for highlighting identifiers.")

(defun merlin--hl-type ()
  (interactive)
  ;; reset verbosity to avoid diplaying deeper types
  (setq merlin--verbosity-cache nil)
  (unless (or (minibufferp)
              (string-match "*" (buffer-name)))
    (if (region-active-p)
        (merlin--type-region)
      (merlin--type-enclosing-query)
      (when merlin-enclosing-types
        (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
          (if (cddr data)
              (if (merlin--is-short (merlin--type-enclosing-text data))
                  (merlin--type-display (cdr data) (merlin--type-enclosing-text data))
                (message "type is too long, check buffer `*merlin-types*' or query the type manually"))))
        (merlin--type-enclosing-after)))))

(defun merlin--hl-is-keyword-p (pos)
  "Check if character at POS is keyword by comparing font face."
  (merlin--hl-current-font-among-fonts-p pos '(tuareg-font-lock-governing-face
                                               font-lock-keyword-face)))

(defun merlin--hl-valid-position-p (pos)
  "Check if POS is in a place valid to get a type"
  (and (not (merlin--hl-in-comment-p pos))
       (or (not (merlin--hl-is-keyword-p pos)) (merlin--hl-in-string-or-doc-p pos))))

(defun merlin--hl-identifiers-function ()
  "Function run after an idle timeout, highlighting the
identifier at point, if necessary."
  (when merlin-hl-identifier-mode
    (let ((symbol (thing-at-point 'symbol)))
      (unless (or (not symbol) (not (merlin--hl-valid-position-p (point))))
        (merlin--hl-type)
        (unless (eq merlin--current-hl-identifier-idle-time merlin-hl-identifier-idle-time)
          (merlin--hl-set-timer))))))

(defun merlin--hl-set-timer ()
  (if merlin--hl-identifier-timer
      (cancel-timer merlin--hl-identifier-timer))
  (setq merlin--current-hl-identifier-idle-time merlin-hl-identifier-idle-time)
  (setq merlin--hl-identifier-timer (run-with-idle-timer
                                     merlin-hl-identifier-idle-time
                                     t
                                     #'merlin--hl-identifiers-function)))

(define-minor-mode merlin-hl-identifier-mode
  "Highlight instances of the identifier at point after a short
timeout."
  :group 'merlin
  (if merlin-hl-identifier-mode
      (unless (or (minibufferp) (string-match "*" (buffer-name)))
        (merlin--hl-set-timer))))

(provide 'merlin-auto-hl)
