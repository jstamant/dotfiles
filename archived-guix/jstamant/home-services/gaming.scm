(define-module (jstamant home-services gaming)
  #:use-module (gnu packages)
  #:use-module (gnu packages emulators)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (nongnu packages game-client)
 #:export (jstamant-gaming-service-type))

(define jstamant-gaming-service-type
  (service-type
    (name 'jstamant-gaming)
    (description "Install gaming-related packages")
    (extensions
     (list
      (service-extension
       home-profile-service-type
       (const (list heroic mednafen)))))
    (default-value #f)))

