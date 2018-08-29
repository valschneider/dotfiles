;; ---------- Setup package archives ----------

;; MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Derived from https://stackoverflow.com/a/10093312/5096023
;; Only need to do that for use-package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ---------- Load & config packages ----------

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-temp-prefix ".flycheck"))

;; Disable the idle time thing
;; (setq flycheck-check-syntax-automatically '(save mode-enable))

(use-package helm
  :ensure t
  :init (setq helm-split-window-inside-p t)
  :config (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-r" . helm-find)))

(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-ag
  :ensure t
  :config
  ;; https://gist.github.com/pesterhazy/fabd629fbb89a6cd3d3b92246ff29779
  (set-variable 'helm-ag-base-command "rg --vimgrep --no-heading --smart-case")
  :bind
  (("C-c C-f" . helm-do-ag-project-root)))

(use-package magit
  :ensure t
  :config
  (setq
   magit-refresh-status-buffer nil
   magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
   ;; Limit the max logcount and don't use --graph
   magit-log-section-arguments (list "-n256" "--decorate")
   magit-log-cutoff-length 50
   magit-revision-insert-related-refs nil))

(use-package ggtags
  :ensure t
  :bind (("M-*" . xref-pop-marker-stack)))

;; TODO: find a way to squash that in the use-package
;; seems like loading ggtags is deferred, so I need this otherwise I have to
;; manually invoke ggtags the first time
(add-hook 'c-mode-hook 'ggtags-mode)
(add-hook 'asm-mode-hook 'ggtags-mode)

(flycheck-set-checker-executable "pylint" "epylint3")
