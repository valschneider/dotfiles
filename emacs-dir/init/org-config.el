;; Prevent agenda from nuking my windows
(setq org-agenda-window-setup 'current-window)

;; Let language major mode take over for indents
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-edit-src-content-indentation 0)

;; Say no to safety
(setq org-confirm-babel-evaluate nil)

;; Say yes to counting stuff
(setq org-hierarchical-checkbox-statistics nil)
(setq org-hierarchical-todo-statistics nil)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
      plantuml-jar-path "/opt/plantuml/plantuml.jar")

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
	 '((python . t)
	   (sh . t)
	   (C . t)
	   (plantuml . t))))

;https://emacs.stackexchange.com/q/3374/19129
(defun org-bg-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to the current emacs theme"
  (when (eq exporter 'html)
    (let ((bg (face-background 'default))
	  (fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
	org-html-head-extra
	(format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
		bg fg))))))

(add-hook 'org-export-before-processing-hook 'org-bg-css-hook)

;; Default CSS for html exporting
(setq org-html-head "<link href=\"http://doc.norang.ca/org.css\" rel=\"stylesheet\"></link>")

(use-package ox-jira
  :load-path "~/.emacs.d/packages/ox-jira/")

;;(require 'ox-confluence)
(use-package ox-confluence)

;; https://tex.stackexchange.com/a/319099
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f"))
