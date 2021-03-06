;; Configuration of mu4e

(load-library "mu4e-thread")

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :init
  (require 'smtpmail)
  (require 'org-mu4e nil t)

  (defun vs/mu4e-compose-mode ()
    (set-variable 'fill-column 75)
    (flyspell-mode)
    (flyspell-buffer))
  :hook
  (mu4e-compose-mode . vs/mu4e-compose-mode)
  :config
  ;; Generic mail setup
  (setq mail-user-agent 'mu4e-user-agent
	user-full-name    "Valentin Schneider"
	user-mail-address "valentin.schneider@arm.com")

  ;; mu4e cosmetics
  (setq mu4e-headers-date-format "%d/%m/%y %R")
  (setq mu4e-split-view 'vertical)
  (setq mu4e-headers-fields (quote
			     ((:human-date . 22)
			      (:flags . 6)
			      (:mailing-list . 10)
			      (:from . 22)
			      (:subject)))))

(defun mu4e-select-contact ()
  (mu4e~request-contacts)
  ;; ??? above is pseudo-asynchronous, better way to wait?
  (while (not mu4e~contacts)
    (sleep-for .05))

  (helm :sources
	(helm-build-sync-source "emails"
	  :candidates
	    (hash-table-keys mu4e~contacts))
	;; :fuzzy-match t
	:buffer "*Email completion*"))

(defun git-commit-select-contact ()
  (interactive)
  (insert (mu4e-select-contact)))

(defun git-commit-read-ident ()
  ;; mu4e gets the names and emails nicely separated, and bunches them together
  ;; however it looks painful/redundant to intercept them in mu4e to store them
  ;; separately. Just split the contact here.
  (let ((contact (mu4e-select-contact)))
    (string-match "\\(.*[[:alnum:]]\\)[[:space:]]*<\\(.*\\)>" contact)
    (list
     (match-string 1 contact)
     (match-string 2 contact))
    ))

(defun mu4e-select-addr ()
  (interactive)
  (let ((contact (mu4e-select-contact)))
    (string-match "\\(.*[[:alnum:]]\\)[[:space:]]*<\\(.*\\)>" contact)
    (insert (match-string 2 contact))))

(fset 'original-git-commit-self-ident (symbol-function 'git-commit-self-ident))

(defun git-commit-self-ident ()
  ;; Certain commit actions (e.g. review) assume they are always done by the
  ;; author, which isn't always true. Piggy back off of read-ident, proposing
  ;; the author ident as default suggestion.
  ;; Bleh, it's actually more painful that I thought. Ideally I would just
  ;; have helm point at some value by default, but I can't see how.
  ;; Luckily I'm at the top of the mu4e email list, so let's go for that.

  ;; (let ((ident (original-git-commit-self-ident)))
  ;;   (git-commit-read-ident (format "%s <%s>" (nth 0 ident) (nth 1 ident)))))
  (git-commit-read-ident))

(defun vs/mu4e-action-copy-msgid (msg)
  (let ((msgid (mu4e-message-field msg :message-id)))
    (when msgid
      (kill-new msgid))))

(defun vs/mu4e-action-git-apply-thread (msg)
  "Apply as patches all messages that are a direct reply to the top mail in the
thread."
  (let ((msg-list ()))
	;; XXX: figure out a non retarded way of getting topmost message-id
	;; (thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
    (mu4e-headers-for-each
     (lambda (msg)
       ;; (message (mu4e-message-field msg :in-reply-to))
       ;; (message thread-id)
       ;; (when (string-equal
       ;;	      ;; Message is directly in-reply-to thread id
       ;;	      (mu4e-message-field msg :in-reply-to)
       ;;	      thread-id)
       (when (and
	      (eq (plist-get (mu4e-message-field msg :thread) :level) 1)
	      (not (string-prefix-p "Re:" (mu4e-message-field msg :subject))))
	 (add-to-list 'msg-list msg))))

    ;; (debug)
    (mapc 'mu4e-action-git-apply-mbox
	  (sort msg-list (lambda (msga msgb)
			   (string<
			    (mu4e-message-field msga :subject)
			    (mu4e-message-field msgb :subject)))))))


(add-to-list 'mu4e-headers-actions '("apply as patch" . mu4e-action-git-apply-mbox) t)
(add-to-list 'mu4e-headers-actions '("Aapply series" . vs/mu4e-action-git-apply-thread) t)
(add-to-list 'mu4e-headers-actions '("mcopy message-id" . vs/mu4e-action-copy-msgid) t)

(defvar vs/mu4e-headers-search-query nil)
(defun vs/mu4e-headers-search-register (query)
  (setq vs/mu4e-headers-search-query query))

(add-hook 'mu4e-headers-search-hook 'vs/mu4e-headers-search-register)

(defun vs/mu4e-headers-main-view-threading ()
  ;; I only want to fold threads in the "main" view, i.e. unfiltered maildir.
  ;; This is my ugly attempt at it. It kinda works.
  (unless (string-match "msgid" vs/mu4e-headers-search-query)
    (mu4e-headers-fold-all)))

(add-hook 'mu4e-headers-found-hook 'vs/mu4e-headers-main-view-threading)

(defun vs/message-tweak-unique-id (id)
  "Replace the standard '.fsf' suffix with a personal touch.
ID is the message-id expected to be created from message-unique-id."
  (replace-regexp-in-string "\.fsf$" ".mognet" id))

(advice-add 'message-unique-id :filter-return #'vs/message-tweak-unique-id)
