(use-package flycheck-tla
  :load-path "~/.emacs.d/packages/flycheck-tla/")

(use-package tla-mode
  :load-path "~/.emacs.d/packages/tla-mode/")

(use-package flycheck-make-c
  :load-path "~/.emacs.d/packages/flycheck-kernel/"
  :config
  (eval-after-load 'flycheck
    '(progn
       (flycheck-make-c-setup)
       (setq flycheck-make-c-jobs "32"))))

(defun flycheck-make-c-toggle-clang ()
  (interactive)
  (setq flycheck-kernel-use-clang (not flycheck-kernel-use-clang)))

(defun flycheck-add-next-checker-safe (checker next-checker)
  "Safely add a checker to the next-checker list (prevents duplicates).

CHECKER is the flycheck checker to which NEXT-CHECKER will be chained."
  (let ((next-checkers (flycheck-checker-get checker 'next-checkers)))
    (if (not (member next-checker next-checkers))
	(flycheck-add-next-checker checker next-checker))))

(use-package flycheck-checkpatch
  :load-path "~/.emacs.d/packages/flycheck-checkpatch/"
  :config
  (eval-after-load 'flycheck
    '(progn
       (flycheck-checkpatch-setup)
       (flycheck-add-next-checker-safe 'make-c 'checkpatch-code))))

(use-package dwarf-mode
  :load-path "~/.emacs.d/packages/dwarf-mode/")

(use-package cocci
  :load-path "~/dotfiles/work/coccinelle/editors/emacs/"
  :mode ("\\.cocci\\'" . cocci-mode))

(use-package ob-rust
  :load-path "~/.emacs.d/packages/ob-rust/")
