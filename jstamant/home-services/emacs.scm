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
    (default-value #f)
    (extensions
     (list
      ;; NOTE I'd love to run the Emacs daemon from Shepherd, but it's too complicated. Instead I've opted to use XDG autostart
      ;; (service-extension
      ;;  home-shepherd-service-type
      ;;  (const
      ;;   (list
      ;;    (shepherd-service
      ;;     (provision '(emacs))
      ;;     (documentation "Run emacs as a user daemon")
      ;;     (start #~(make-forkexec-constructor
      ;;               (list #$(file-append emacs "/bin/emacs") "--fg-daemon")
      ;;               ;; #:user "jstamant" ; add this did not fix the path for finding eln files
      ;;               ;; #:directory "/home/jstamant/.emacs.d" ; didn't change the path for finding eln files
      ;;               #:log-file "/home/jstamant/.local/var/log/emacs.log"))
      ;;     (stop #~(make-kill-destructor))))))
      (service-extension
       home-profile-service-type
       (const (list emacs-pgtk))))))) ; emacs-pgtk is the emacs package for wayland support

