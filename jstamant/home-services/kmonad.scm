(define-module (jstamant home-services kmonad)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (jstamant-kmonad-service-type))

(define jstamant-kmonad-service-type
  (service-type
   (name 'jstamant-kmonad)
   (description "Run kmonad as a user daemon")
   (default-value #f)
   (extensions
    (list
     (service-extension
      home-shepherd-service-type
      (const
       (list
        (shepherd-service
         (provision '(kmonad))
         (documentation "Run kmonad as a user daemon")
         (start #~(make-forkexec-constructor
                   (list #$(file-append kmonad "/bin/kmonad")
                         "/home/jstamant/.config/kmonad/config.kbd")
                   #:user "jstamant"
                   #:log-file "/home/jstamant/.local/var/log/kmonad.log"))
         (stop #~(make-kill-destructor))))))
     (service-extension
      home-profile-service-type
      (const (list kmonad)))))))
