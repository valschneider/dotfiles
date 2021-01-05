;; Configuration of notmuch

;; From the doc (https://notmuchmail.org/emacstips/):
;;
;; It is not recommended to install Notmuch Emacs from the Emacs Lisp Package
;; Archive (ELPA), as the version there is likely not in sync with the command
;; line interface.

(use-package notmuch
  :init
  (defun vs/notmuch-tree-narrow-query ()
    "Narrow the current tree search with a further query."
    (interactive)
    (notmuch-tree
     (notmuch-tree-get-query)
     (notmuch-read-query "Notmuch tree narrow query: ")))

  (defun vs/notmuch-tree-thread-at-point ()
    "Get the thread-id of message at point."
    ;; Below query should work for newer versions :'(
    ;; (concat "thread:{id:" (notmuch-show-stash-message-id) "}"))
    (car (process-lines
	  notmuch-command
	  "search"
	  "--output=threads"
	  (notmuch-show-get-message-id))))

  (defun vs/notmuch-tree-narrow-to-thread ()
    "Narrow the current tree search to the highlighted thread."
    (interactive)
    (notmuch-tree
     (notmuch-tree-get-query)
     (vs/notmuch-tree-thread-at-point)))

  (defvar vs/notmuch-tree-narrow-map
    (let ((map (make-sparse-keymap)))
      (define-key map "q" 'vs/notmuch-tree-narrow-query)
      (define-key map "t" 'vs/notmuch-tree-narrow-to-thread)
      map))

  (fset 'vs/notmuch-tree-narrow-map vs/notmuch-tree-narrow-map)

  (defun vs/notmuch-select-contact ()
    (interactive)
    (insert
     (helm :sources
	   (helm-build-sync-source "emails"
	     :candidates
	     (notmuch-address-options ""))
	   ;; :fuzzy-match t
	   :buffer "*Email completion*")))

  (defun vs/notmuch-tree-toggle-tag (tag)
    "Toggle a given tag for the message at point.
TAG is the tag to toggle."
    (if (member tag (notmuch-tree-get-tags))
	(notmuch-tree-tag (list (concat "-" tag)))
      (notmuch-tree-tag (list (concat "+" tag)))))

  (defun vs/notmuch-tree-toggle-thread-tag (tag)
    "Toggle a given tag for the thread at point.

TAG is the tag to toggle."
    (if (member tag (notmuch-tree-get-tags))
	(notmuch-tree-tag-thread (list (concat "-" tag)))
      (notmuch-tree-tag-thread (list (concat "+" tag)))))

  :bind (:map notmuch-tree-mode-map
	      ("i" . (lambda () (interactive) (vs/notmuch-tree-toggle-tag "inbox")))
	      ("I" . (lambda () (interactive) (vs/notmuch-tree-toggle-thread-tag "inbox")))
	      ("t" . (lambda () (interactive) (vs/notmuch-tree-toggle-tag "track")))
	      ("T" . (lambda () (interactive) (vs/notmuch-tree-toggle-thread-tag "track"))))
  ;; XXX: cannot specify keymap in :bind-keymap ?
  :config (define-key notmuch-tree-mode-map "n" 'vs/notmuch-tree-narrow-map)
  ;; :bind-keymap (:map notmuch-tree-mode-map
	      ;; ("n" . vs/notmuch-tree-narrow-map))
  )

(use-package ol-notmuch)

;; XXX UPSTREAM ME XXX
(defun vs/notmuch-tree-worker (basic-query &optional query-context target open-target)
  "Insert the tree view of the search in the current buffer.

This is is a helper function for notmuch-tree. The arguments are
the same as for the function notmuch-tree."
  (interactive)
  (notmuch-tree-mode)
  (add-hook 'post-command-hook #'notmuch-tree-command-hook t t)
  (setq notmuch-tree-basic-query basic-query)
  (setq notmuch-tree-query-context query-context)
  (setq notmuch-tree-target-msg target)
  (setq notmuch-tree-open-target open-target)
  ;; Set the default value for `notmuch-show-process-crypto' in this
  ;; buffer. Although we don't use this some of the functions we call
  ;; (such as reply) do. It is a buffer local variable so setting it
  ;; will not affect genuine show buffers.
  (setq notmuch-show-process-crypto notmuch-crypto-process-mime)

  (erase-buffer)
  (goto-char (point-min))
  (let* ((search-args (if query-context
			  (concat "(" basic-query ") and (" query-context ")")
			basic-query))
	 (message-arg "--entire-thread"))
    (if (equal (car (process-lines notmuch-command "count" search-args)) "0")
	(setq search-args basic-query))
    (notmuch-tag-clear-cache)
    (let ((proc (notmuch-start-notmuch
		 "notmuch-tree" (current-buffer) #'notmuch-tree-process-sentinel
		 "show" "--body=false" "--format=sexp"
		 message-arg search-args))
	  ;; Use a scratch buffer to accumulate partial output.
	  ;; This buffer will be killed by the sentinel, which
	  ;; should be called no matter how the process dies.
	  (parse-buf (generate-new-buffer " *notmuch tree parse*")))
      (process-put proc 'parse-buf parse-buf)
      (set-process-filter proc 'notmuch-tree-process-filter)
      (set-process-query-on-exit-flag proc nil))))

(advice-add 'notmuch-tree-worker :override #'vs/notmuch-tree-worker)
;; XXX UPSTREAM ME XXX

;; addr
(rfc822-addresses "Valentin Schneider <valentin.schneider@arm.com>")