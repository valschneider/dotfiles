(defun vs/message-mode-hook ()
  (set-variable 'fill-column 75)
  (flyspell-mode)
  (flyspell-buffer))
(add-hook 'message-mode-hook 'vs/message-mode-hook)

(setq user-full-name    "Valentin Schneider"
      user-mail-address "vschneid@redhat.com"

      ;; XXX: there's a nice message-cite-style-thunderbird, but
      ;; a) it doesn't work, b) it uses the retarded m/d/y format
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "On %d/%m/%y %R, %N wrote:")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-user "vschneid@redhat.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      smtpmail-debug-info t)

(defun vs/message-tweak-unique-id (id)
  "Replace the standard '.fsf' suffix with a personal touch.
ID is the message-id expected to be created from message-unique-id."
  (replace-regexp-in-string "\.fsf$" ".mognet" id))
(advice-add 'message-unique-id :filter-return #'vs/message-tweak-unique-id)

;; (defun vs/message-unique-id ()
;;   "Do something more like thunderbird with current date + some random bit."
;;   ;; [YYYYMMDDhhmmss.<blurb>-valsch@arm.com]
;;   )

(defun vs/cycle-list-value (list current)
  (car (cdr (member current (nconc (copy-tree list) list)))))

(defvar vs/smtpmail-servers '("foss.arm.com" "smtp.office365.com"))

(defun vs/cycle-smtp-server ()
  "Cycle between outgoing SMTP servers."
  (interactive)
  (let ((next (vs/cycle-list-value vs/smtpmail-servers smtpmail-smtp-server)))
    (message "Switching to SMTP server: %s" next)
    (set-variable 'smtpmail-smtp-server next)))
