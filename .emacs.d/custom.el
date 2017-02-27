(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-in-non-selected-windows t)
 '(cursor-type (quote box))
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "21b6f06754acc3ea0bf1c325f373595cc824b641672bd944c9e66feb10a08da7" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fci-rule-color "#14151E")
 '(ledger-reports
   (quote
    (("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(package-selected-packages
   (quote
    (systemd wanderlust scad-mode "use-package" use-package-chords ledger ledger-mode evil-visual-mark-mode evil web-mode use-package smart-mode-line scad-preview pkgbuild-mode markdown-mode magit auctex)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((min-colors 256)) (:foreground "gainsboro" :background "grey18" :height 100)) (t (:foreground "gainsboro" :background "grey18" :height 100))))
 '(font-lock-comment-face ((((min-colors 256)) (:foreground "CadetBlue3")) (t (:foreground "CadetBlue3"))))
 '(font-lock-constant-face ((((min-colors 256)) (:foreground "salmon1")) (t (:foreground "salmon1"))))
 '(font-lock-keyword-face ((((min-colors 256)) (:foreground "plum3")) (t (:foreground "plum3"))))
 '(font-lock-string-face ((((min-colors 256)) (:foreground "DarkSeaGreen3")) (t (:foreground "DarkSeaGreen3"))))
 '(link ((((min-colors 256)) (:foreground "CadetBlue3")) (t (:foreground "CadetBlue3"))))
 '(org-done ((((min-colors 256)) (:foreground "DarkSeaGreen3")) (t (:foreground "DarkSeaGreen3"))))
 '(org-todo ((((min-colors 256)) (:foreground "tomato")) (t (:foreground "tomato"))))
 '(outline-1 ((((min-colors 256)) (:foreground "CornflowerBlue")) (t (:foreground "CornflowerBlue"))))
 '(outline-2 ((((min-colors 256)) (:foreground "gold")) (t (:foreground "gold"))))
 '(term-color-black ((((min-colors 256)) (:foreground "grey18")) (t (:foreground "grey18"))))
 '(term-color-blue ((((min-colors 256)) (:foreground "CornflowerBlue")) (t (:foreground "CornflowerBlue"))))
 '(term-color-cyan ((((min-colors 256)) (:foreground "CadetBlue3")) (t (:foreground "CadetBlue3"))))
 '(term-color-green ((((min-colors 256)) (:foreground "DarkSeaGreen3")) (t (:foreground "DarkSeaGreen3"))))
 '(term-color-magenta ((((min-colors 256)) (:foreground "plum3")) (t (:foreground "plum3"))))
 '(term-color-red ((((min-colors 256)) (:foreground "tomato")) (t (:foreground "tomato"))))
 '(term-color-white ((((min-colors 256)) (:foreground "gainsboro")) (t (:foreground "gainsboro"))))
 '(term-color-yellow ((((min-colors 256)) (:foreground "gold")) (t (:foreground "gold")))))
