(define-module (jstamant home-services sway)
  #:use-module (gnu home services)
  #:use-module (gnu home services sway)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (jstamant-sway-configuration))


(define jstamant-sway-configuration
  (sway-configuration
    (packages (list alacritty rofi sway waybar))
    (variables `((mod   . "Mod4")
                 (left  . "h")
                 (down  . "j")
                 (up    . "k")
                 (right . "l")
                 (term  . ,(file-append alacritty "/bin/alacritty"))
                 (launcher . ,(file-append rofi "/bin/rofi -show drun"))))
    (keybindings (cons* '($mod+d . "exec $launcher")
                        (filter
                         (lambda (binding) (if (eqv? (car binding) '$mod+d) #f #t))
                         %sway-default-keybindings)))
    (modes %sway-default-modes)
    (gestures %sway-default-gestures)
    (inputs '())
    (outputs '())
    (startup+reload-programs '())
    (startup-programs %sway-default-startup-programs)
    (extra-content '())))


;; TODO implement my own sway service-type when home-sway-service-type is better

;; home-sway-service-type's extension sucks right now, it appends my
;; configuration to the default configuration, which is not sensible.
;; This will likely change soon, as it was only implemented recently

;; (define jstamant-sway-service-type
;;   (service-type
;;     (name 'jstamant-sway)
;;     (description "Install and configure Sway")
;;     (extensions
;;      (list
;;       (service-extension
;;        home-profile-service-type
;;        (lambda (_) (list alacritty rofi sway waybar)))
;;       (service-extension
;;        home-sway-service-type
;;        (lambda (_)
;;          (sway-configuration
;;            (variables `((mod   . "Mod4")
;;                         (left  . "h")
;;                         (down  . "j")
;;                         (up    . "k")
;;                         (right . "l")
;;                         (term  . ,(file-append alacritty "/bin/alacritty"))
;;                         (launcher . ,(file-append rofi "/bin/rofi -show drun"))))
;;            (keybindings (cons* '($mod+d . "exec $launcher")
;;                                (filter
;;                                 (lambda (binding) (if (eqv? (car binding) '$mod+d) #f #t))
;;                                 %sway-default-keybindings))))))))
;;     (default-value #f)))
