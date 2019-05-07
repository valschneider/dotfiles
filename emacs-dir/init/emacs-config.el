;; Configuration of emacs itself

;; GUIs are for the weak
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Show line number
(global-linum-mode t)
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

;; Delete whitespaces on saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Local toggle
(defun toggle-whitespace-cleanup()
  (interactive)

  ;; FIXME: this wants to use buffer local hook
  (if (member 'whitespace-cleanup before-save-hook)
      (progn
	(remove-hook 'before-save-hook 'whitespace-cleanup nil)
	(message "whitespace-cleanup disabled"))
    (progn
      (add-hook 'before-save-hook 'whitespace-cleanup nil nil)
      (message "whitespace-cleanup enabled"))))

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

(setq c-default-style "linux")