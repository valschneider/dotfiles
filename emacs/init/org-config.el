(require 'org)
(require 'ox)

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
(setq org-checkbox-hierarchical-statistics nil)
(setq org-hierarchical-todo-statistics nil)

(defun org-mode-custom-setup()
  (set-variable 'fill-column 80)
  (auto-fill-mode))

(add-hook 'org-mode-hook 'org-mode-custom-setup)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
      plantuml-jar-path "/opt/plantuml/plantuml.jar")

(setq org-ditaa-jar-path "/opt/ditaa/ditaa.jar")

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
	 '((python . t)
	   (shell . t)
	   (C . t)
	   (plantuml . t)
	   (ditaa . t))))

(setq org-file-apps
      '((auto-mode . emacs)
	("\\.x?html?\\'" . "firefox %s")
	("\\.pdf\\'" . "evince \"%s\"")
	("\\.dat\\'" . "kernelshark %s")))

;; https://emacs.stackexchange.com/q/3374/19129
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

;; Oh boy, fun stuff up ahead
;; LaTeX code rendering is awful, and getting it to work the way I want is a
;; war I do not want to fight. HTML rendering of org code snippets is just the
;; way I like it, so the idea is give LaTeX the finger and generate HTML instead,
;; then convert it to something shit-for-brains LaTeX can display.
;;
;; TODO: crop pdf cause it takes a whole page ATM
;; TODO: needs exports none when creating slides, but exports code when
;; generating
;; TODO: disable/enable theme (special func maybe?)
(defun latex-html-src-block (src-block contents info)
  (let ((out-file (org-export-read-attribute :attr_htmltopdf src-block :file)))
    (if (not out-file)
	(org-export-with-backend 'latex src-block contents info)

      (let* ((out-file (org-export-read-attribute :attr_htmltopdf src-block :file))
	     (in-file (org-babel-temp-file "ltxhtml-" ".html"))

	     (full-body (concat
			 ;; TODO: this is a manual slide-in of org-bg-css-hook,
			 ;; clean that up
			 (org-export-with-backend 'html src-block contents info)
			 (let ((bg (face-background 'default))
			       (fg (face-foreground 'default)))

			   (concat
			    org-html-head-extra
			    (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
				    bg fg)))))

	     (cmd
	      (concat "wkhtmltopdf "
		      ;; Remove all margins
		      "-B 0 -L 0 -R 0 -T 0 "
		      ;; "--disable-smart-shrinking"
		      (org-babel-process-file-name in-file)
		      " "
		      (org-babel-process-file-name out-file))))
	      ;; (concat "google-chrome "
	      ;;	      "--headless "
	      ;;	      "--disable-gpu "
	      ;;	      "--no-margins "
	      ;;	      "--print-to-pdf=" (org-babel-process-file-name out-file)
	      ;;	      " " (org-babel-process-file-name in-file))))

	(with-temp-file in-file (insert full-body))
	(message "%s" cmd) (org-babel-eval cmd "")))))

(org-export-define-derived-backend 'latex-html 'latex
  :translate-alist '((src-block . latex-html-src-block)))

(defun org-export-pdfize-code ()
  (interactive)
  (org-export-to-buffer 'latex-html "pdfize"))

;; Default CSS for html exporting
(setq org-html-head "<link href=\"http://doc.norang.ca/org.css\" rel=\"stylesheet\"></link>")

(use-package ox-jira
  :load-path "~/.emacs.d/packages/ox-jira/")

;;(require 'ox-confluence)
(use-package ox-confluence)

;; https://tex.stackexchange.com/a/319099
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f"))
