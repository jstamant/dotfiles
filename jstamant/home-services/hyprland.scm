(define-module (jstamant home-services hyprland)
  #:use-module (gnu home services)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (jstamant-hyprland-service-type))

(define jstamant-hyprland-service-type
  (service-type
   (name 'jstamant-hyprland)
   (description "Install and configure Hyprland")
   (extensions
    (list
     (service-extension
      home-profile-service-type
      (lambda (_) (list alacritty hyprland rofi waybar)))
     (service-extension
      home-xdg-configuration-files-service-type
      (lambda (_) `(("hypr/hyprland.conf" ,(local-file "hyprland/hyprland.conf")))))))
   (default-value #f)))
