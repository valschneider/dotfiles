(use-package flycheck-tla
  :load-path "~/.emacs.d/packages/flycheck-tla/")

(use-package tla-mode
  :load-path "~/.emacs.d/packages/tla-mode/")

(use-package flycheck-kernel
  :load-path "~/.emacs.d/packages/flycheck-kernel/"
  :config
  (eval-after-load 'flycheck
    '(progn
       (flycheck-kernel-setup)
       (setq flycheck-kernel-make-jobs "32"))))

(defun flycheck-kernel-toggle-clang ()
  (interactive)
  (setq flycheck-kernel-use-clang (not flycheck-kernel-use-clang)))

(use-package flycheck-checkpatch
  :load-path "~/.emacs.d/packages/flycheck-checkpatch/"
  :config
  (eval-after-load 'flycheck
    '(progn
       (flycheck-checkpatch-setup)
       (flycheck-add-next-checker 'kernel-make 'checkpatch-code))))
