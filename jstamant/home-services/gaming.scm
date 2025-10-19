(define-module (jstamant home-services gaming)
  #:use-module (gnu packages)
  #:use-module (gnu packages emulators)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:export (jstamant-gaming-service-type))

(define jstamant-gaming-service-type
  (service-type
    (name 'jstamant-gaming)
    (description "Install gaming-related packages")
    (extensions
     (list
      (service-extension
       home-profile-service-type
       (lambda (_) (list mednafen)))))
    (default-value #f)))

