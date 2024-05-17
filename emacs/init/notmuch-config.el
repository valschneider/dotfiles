;; Configuration of notmuch

;; From the doc (https://notmuchmail.org/emacstips/):
;;
;; It is not recommended to install Notmuch Emacs from the Emacs Lisp Package
;; Archive (ELPA), as the version there is likely not in sync with the command
;; line interface.

;; XXX have a git package somewhere?
(defun vs/git-apply-patch (file directory)
  "Apply a patch file to a git repository.
FILE the file to apply - any format accepted by $(git am).
DIRECTORY the root directory of the git repository."
  (shell-command (format "cd %s; git am -3 '%s'" directory file)))

(use-package notmuch
  :init
  (setq notmuch-address-command (concat (file-name-directory load-file-name) "./notmuch-addresses"))

  (defun vs/notmuch-tree-narrow-query ()
    "Narrow the current tree search with a further query."
    (interactive)
    (notmuch-tree
     (notmuch-tree-get-query)
     (notmuch-read-query "Notmuch tree narrow query: ")))

  (defun vs/notmuch-matching-threads (query)
    "Get the threads matching a given query
QUERY is the given notmuch query."
    (process-lines
     notmuch-command
     "search"
     "--output=threads"
     query))

  (defun vs/notmuch-hello-insert-tracked-searches ()
    (let ((searches (notmuch-hello-query-counts
		     `(("Tracked patches" . ,(string-join
					      (vs/notmuch-matching-threads "tag:track")
					      " or ")))
		     :show-empty-searches notmuch-show-empty-saved-searches)))
      (when searches
	(widget-insert "Dynamic queries: ")
	(widget-insert "\n\n")
	(let ((start (point)))
	  (notmuch-hello-insert-buttons searches)
	  (indent-rigidly start (point) notmuch-hello-indent)))))

  (defun vs/notmuch-tree-narrow-to-thread ()
    "Narrow the current tree search to the highlighted thread."
    (interactive)
    (notmuch-tree
     (notmuch-tree-get-query)
     (car (vs/notmuch-matching-threads (notmuch-show-get-message-id)))))

  (defvar vs/notmuch-tree-narrow-map
    (let ((map (make-sparse-keymap)))
      (define-key map "q" 'vs/notmuch-tree-narrow-query)
      (define-key map "t" 'vs/notmuch-tree-narrow-to-thread)
      map))

  (fset 'vs/notmuch-tree-narrow-map vs/notmuch-tree-narrow-map)

  (defun vs/notmuch-select-contact ()
     (helm :sources
	   (helm-build-sync-source "emails"
	     :candidates
	     (notmuch-address-matching ""))
	   ;; :fuzzy-match t
	   :buffer "*Email completion*"))

  (defun vs/notmuch-insert-contact ()
    (interactive)
    (insert (vs/notmuch-select-contact)))

  (defun vs/git-commit-read-ident (&optional prompt)
    ;; XXX rfc822-addresses()
    (let ((contact (vs/notmuch-select-contact)))
      (string-match "\\(.*[[:alnum:]]\\)[[:space:]]*<\\(.*\\)>" contact)
      (list
       (match-string 1 contact)
       (match-string 2 contact))
      ))
  (advice-add 'git-commit-self-ident :override  #'vs/git-commit-read-ident)
  (advice-add 'git-commit-read-ident :override  #'vs/git-commit-read-ident)

  (defun vs/notmuch-apply-patch ()
    (interactive)
    (vs/git-apply-patch (notmuch-show-get-filename)
			(expand-file-name (read-directory-name "Git directory: " nil nil t))))

  (defun vs/notmuch-tree-get-depth ()
    "Get the depth of the message at point in the thread.
0-based, as all things should be."
    ;; FIXME: please tell me there is a depth indicator that doesn't have to rely
    ;; display attributes, this makes me wanna puke...
    (- (length (plist-get (notmuch-tree-get-message-properties) :tree-status)) 2))

  (defun vs/notmuch-tree-get-subject ()
    (plist-get (notmuch-tree-get-prop :headers) :Subject))

  (defun vs/notmuch-apply-thread ()
    ;; Iterate over first descendent of cover letter; apply
    (interactive)
    (let ((directory
	   (expand-file-name (read-directory-name "Git directory: " nil nil t)))
	  (first (save-excursion (progn
				  (notmuch-tree-thread-top)
				  (vs/notmuch-tree-get-subject)))))

      (notmuch-tree-thread-mapcar
       (lambda ()
	 (when (and
		(= (vs/notmuch-tree-get-depth) 1)
		(not (string-prefix-p "Re:" (vs/notmuch-tree-get-subject))))
	   (vs/git-apply-patch (notmuch-show-get-filename) directory))))
      )
    )

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
	      ("i" . (lambda () (interactive) (vs/notmuch-tree-toggle-tag "inbox")        (notmuch-tree-refresh-view)))
	      ("I" . (lambda () (interactive) (vs/notmuch-tree-toggle-thread-tag "inbox") (notmuch-tree-refresh-view)))
	      ("t" . (lambda () (interactive) (vs/notmuch-tree-toggle-tag "track")        (notmuch-tree-refresh-view)))
	      ("T" . (lambda () (interactive) (vs/notmuch-tree-toggle-thread-tag "track") (notmuch-tree-refresh-view))))
  ;; XXX: cannot specify keymap in :bind-keymap ?
  :config
  (define-key notmuch-tree-mode-map "n" 'vs/notmuch-tree-narrow-map)
  ;; XXX: version >= 0.24
  ;; Use the default saving behaviour (doesn't involve notmuch database)
  (define-key notmuch-message-mode-map (kbd "C-x C-s") nil)
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

;; (advice-add 'notmuch-tree-worker :override #'vs/notmuch-tree-worker)
;; XXX UPSTREAM ME XXX

;; addr
(rfc822-addresses "Valentin Schneider <valentin.schneider@arm.com>")
