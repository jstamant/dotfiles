;;; lang/cfml/config.el -*- lexical-binding: t; -*-

(use-package! cfml-mode
  :mode (("\\.cfc\\'" . cfscript-mode)
         ("\\.cfm\\'" . cfscript-mode)))

;; TODO implement apheleia or a formatter!
(after! apheleia
  ;; For some reason, running box fmt without --overwrite adds an
  ;; extra newline at the end of the file
  (add-to-list 'apheleia-formatters '(cfformat "box" "fmt" filepath "--overwrite"))
  ;; (add-to-list 'apheleia-formatters '(cfformat "box" "fmt" inplace "--overwrite"))
  (add-to-list 'apheleia-mode-alist '(cfml-mode . cfformat))
  (add-to-list 'apheleia-mode-alist '(cfscript-mode . cfformat)))

(defun cfml-format ()
  "Formats the contents of a buffer using `box'"
  (interactive)
  (shell-command-on-region
   (point-min)
   (1- (point-max))
   (concat "box fmt " buffer-file-name)
   ;; Output buffer
   (current-buffer)
   'replace-contents
   ;; Name of the error buffer
   "*Box Format Error Buffer*"
   'show-error-buffer)
  (goto-char (- (point-max) 3))
  (delete-char 3))
