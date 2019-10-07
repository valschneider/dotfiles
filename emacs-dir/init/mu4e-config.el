;; Configuration of mu4e

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :config
  ;; Generic mail setup
  (setq mail-user-agent 'mu4e-user-agent

	user-full-name    "Valentin Schneider"
	user-mail-address "valentin.schneider@arm.com")

  ;; mu4e cosmetics
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M"))

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
