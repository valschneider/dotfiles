;; Configuration of emacs itself

;; GUIs are for the weak
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Show column number
(setq column-number-mode t)

;; Colour theme
(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

;; Don't put anything in scratch
(setq initial-scratch-message nil)

;; 3 letters is too much
(fset 'yes-or-no-p 'y-or-n-p)

;; Change backup (~) behaviour
(setq backup-directory-alist `(("." . "~/.emacs_saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Make selection sane
(delete-selection-mode t)

;; Automatic whitespace cleanup
(defvar save-cleanup-enabled t)
(defvar no-save-cleanup-list (list 'org-mode 'patch-mode))
(defun save-cleanup()
  (unless (or
	   (member major-mode no-save-cleanup-list)
	   (not save-cleanup-enabled))
    (whitespace-cleanup)))

(defun toggle-whitespace-cleanup()
  (interactive)

  ;; FIXME: this should be buffer local
  (if save-cleanup-enabled
      (progn
	(setq save-cleanup-enabled nil)
	(message "whitespace-cleanup disabled"))
    (progn
      (setq save-cleanup-enabled t)
      (message "whitespace-cleanup enabled"))))

;; Delete whitespaces on saving
(add-hook 'before-save-hook 'save-cleanup)

;; Code-folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; https://www.reddit.com/r/emacs/comments/746cd0/which_code_folding_package_do_you_use/dnwi2x1
(defun toggle-folding ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))

(global-set-key (kbd "C-c C-<tab>") 'toggle-folding)

(defun reload-emacs()
  (interactive)
  (load-file "~/.emacs"))

(add-hook 'c-mode-hook (lambda () (set-variable 'fill-column 80)))
(setq c-default-style "linux")

(add-hook 'git-commit-setup-hook (lambda () (set-variable 'fill-column 75)))
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;; Auto chmod +x
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun get-git-ref-at-point ()
  (interactive)
  (let ((msg
    (format "%s:%s @l%i"
	    (magit-rev-abbrev (magit-headish))
	    (magit-current-file)
	    (line-number-at-pos))))
    (kill-new msg)
    (message "%s" msg)))

(defun magit-ref-at-point ()
  ;; Stolen from magit-show-commit
  (or (and (bound-and-true-p magit-blame-mode)
	   (magit-current-blame-chunk)
	   (magit-branch-or-commit-at-point)
      (magit-commit-at-point))))

(defun magit-copy-ref-at-point ()
  (interactive)
  (kill-new (magit-commit-at-point)))

(defun magit-copy-kref-at-point ()
  (interactive)
  ;; (message "%s" (magit-ref-at-point)))
  (kill-new (magit-git-str "kshow" (magit-commit-at-point))))

;; https://stackoverflow.com/a/5194503
(defun remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match " " nil t))))

;; https://delta.re/org-screenshot/
(defun org-insert-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
	(concat
	 (make-temp-name
	  (concat (file-name-nondirectory (buffer-file-name))
		  "_imgs/"
		  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]"))))
