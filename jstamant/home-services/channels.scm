(define-module (jstamant home-services channels)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:export (jstamant-channels-service-type))

(define jstamant-channels
  #~(list
     (channel
       (name 'guix)
	   (url "https://codeberg.org/guix/guix.git")
       (branch "master")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
     (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

;; 2025-10-10 the default url for guix channel is git.guix.gnu.org...
;; The home-channels-service-type deletes my change to the guix channel url...
;; TODO implement this and change my channels once the default Guix url is changed to codeberg upstream (around mid-2026)
;; (define jstamant-channels-service-type
;;   (service-type
;;     (name 'jstamant-channels)
;;     (description "Set my Guix channels")
;;     (default-value jstamant-channels)
;;     (extensions
;;      (list (service-extension
;;             home-channels-service-type
;;             (const jstamant-channels))))))

(define jstamant-channels-service-type
  (service-type
    (name 'jstamant-channels)
    (description "Set my Guix channels")
    (default-value jstamant-channels)
    (extensions
     (list (service-extension
            home-xdg-configuration-files-service-type
            (const `(("guix/channels.scm" ,(scheme-file "channels.scm" jstamant-channels)))))))))

