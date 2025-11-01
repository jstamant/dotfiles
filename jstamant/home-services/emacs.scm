(define-module (jstamant home-services emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:export (jstamant-emacs-service-type))

(define jstamant-emacs-service-type
  (service-type
    (name 'jstamant-emacs)
    (description "Install Emacs and required packages")
    (extensions
     (list
      (service-extension
       home-profile-service-type
       (lambda (_) (list emacs-pgtk))))) ; emacs-pgtk is the emacs package for wayland support
    (default-value #f)))

