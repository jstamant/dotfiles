(define-module (jstamant home-services emacs)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services))

(define-public home-emacs-service-type
  (service-type
   (name 'home-emacs)
   (description "Install Emacs and required packages")
   (extensions
    (list
     (service-extension
      home-profile-service-type
      (lambda (_) (list (specification->package "emacs"))))))
   (default-value #f)))

