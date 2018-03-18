(require 'merlin)

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
        (merlin--type-enclosing-after))))
  )

(defun merlin--hl-identifiers-function ()
  "Function run after an idle timeout, highlighting the
identifier at point, if necessary."
  (when merlin-hl-identifier-mode
    (merlin--hl-type)
    (unless (eq merlin--current-hl-identifier-idle-time merlin-hl-identifier-idle-time)
      (merlin--hl-set-timer))))

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
