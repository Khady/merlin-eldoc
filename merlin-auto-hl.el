(require 'merlin)

(defcustom merlin-auto-hl-idle-time 0.50
  "How long to wait after user input before highlighting the current identifier."
  :type 'float
  :group 'merlin-auto-hl)

(defvar merlin-auto-hl--identifier-timer
  nil
  "The timer used for highlighting identifiers.")

(defvar-local merlin-auto-hl--previous-point
  nil
  "The value of the point last time this mode displayed information.")

;;; Utils

;; imported from evil-matchit
(defun merlin-auto-hl--current-font-among-fonts-p (pos fonts)
  "If current font at POS is among FONTS."
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar (lambda (f)
                    (member f fonts))
                  fontfaces))))

(defun merlin-auto-hl--in-comment-p (pos)
  "Check character at POS is comment by comparing font face."
  (merlin-auto-hl--current-font-among-fonts-p pos '(font-lock-comment-face
                                                    font-lock-comment-delimiter-face)))


(defun merlin-auto-hl--in-string-or-doc-p (pos)
  "Check character at POS is string or document by comparing font face."
  (merlin-auto-hl--current-font-among-fonts-p pos '(font-lock-string-face
                                                    font-lock-doc-face)))

(defun merlin-auto-hl--is-keyword-p (pos)
  "Check if character at POS is keyword by comparing font face."
  (merlin-auto-hl--current-font-among-fonts-p pos '(tuareg-font-lock-governing-face
                                                    font-lock-keyword-face)))

(defun merlin-auto-hl--valid-position-p (pos)
  "Check if POS is in a place valid to get a type"
  (and (not (merlin-auto-hl--in-comment-p pos))
       (or (not (merlin-auto-hl--is-keyword-p pos)) (merlin-auto-hl--in-string-or-doc-p pos))))

;;; Main logic

(defun merlin-auto-hl--type ()
  (interactive)
  ;; reset verbosity to not display deeper types
  (setq merlin--verbosity-cache nil)
  (if (region-active-p)
      (merlin--type-region)
    (merlin--type-enclosing-query)
    (when merlin-enclosing-types
      (let ((data (elt merlin-enclosing-types merlin-enclosing-offset)))
        (if (cddr data)
            (if (merlin--is-short (merlin--type-enclosing-text data))
                (merlin--type-display (cdr data) (merlin--type-enclosing-text data))
              (message "type is too long, check buffer `*merlin-types*' or query the type manually"))))
      (merlin--type-enclosing-after))))

(defun merlin-auto-hl--identifiers-function ()
  "Function run after an idle timeout, highlighting the
identifier at point, if necessary."
  (when (and merlin-auto-hl-mode                    ; current buffer has the mode on
             (not (string-match "*" (buffer-name))) ; avoid buffers like `*merlin-types*'
             (not (minibufferp)))
    (let ((symbol (thing-at-point 'symbol))
          (pos (point)))
      (unless (or (eq pos merlin-auto-hl--previous-point)
                  (not symbol)
                  (not (merlin-auto-hl--valid-position-p pos)))
        (setq-local merlin-auto-hl--previous-point pos)
        (merlin-auto-hl--type)
        (unless (eq merlin-auto-hl--current-hl-identifier-idle-time merlin-auto-hl-idle-time)
          (merlin-auto-hl--set-timer))))))

(defun merlin-auto-hl--cancel-timer ()
  (if merlin-auto-hl--identifier-timer
      (cancel-timer merlin-auto-hl--identifier-timer)))

(defun merlin-auto-hl--set-timer ()
  (merlin-auto-hl--cancel-timer)
  (setq merlin-auto-hl--current-hl-identifier-idle-time merlin-auto-hl-idle-time)
  (setq merlin-auto-hl--identifier-timer (run-with-idle-timer
                                          merlin-auto-hl-idle-time
                                          t
                                          #'merlin-auto-hl--identifiers-function)))

;;;###autoload
(define-minor-mode merlin-auto-hl-mode
  "Highlight instances of the identifier at point after a short
timeout."
  nil nil nil
  (if merlin-auto-hl-mode
      (merlin-auto-hl--set-timer)
    (merlin-auto-hl--cancel-timer)))

(provide 'merlin-auto-hl)
