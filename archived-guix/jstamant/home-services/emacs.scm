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
      ;; NOTE I'd love to run the Emacs daemon from Shepherd, but
      ;; there's a bug in the way Emacs handles its argv[0] when it
      ;; gets called. This is an Emacs bug, but Guix might implement a
      ;; fix in the next core update:
      ;; https://lists.gnu.org/archive/html/guix-patches/2025-07/msg00181.html
      ;; Instead I've opted to use XDG autostart :(

      ;; https://codeberg.org/guix/guix/issues/1674

;; (define* (emacs #:key (name 'server)
;;                 (uid (getuid))
;;                 (gid (getgid))
;;                 (options '()))
;;   (service (list (symbol-append 'emacs@ name))
;;     #:start
;;     (make-systemd-constructor
;;      (cons* "emacs" "--fg-daemon" options)
;;      (list (endpoint
;;             (make-socket-address AF_UNIX
;;                                  (format #f "/run/user/~d/emacs/~s"
;;                                          uid name))
;;             #:socket-directory-permissions #o700
;;             #:socket-owner uid
;;             #:socket-group gid)))
;;     #:stop (make-systemd-destructor)
;;     #:respawn? #t))

      ;; (map
      ;;                   (lambda (var)
      ;;                     (if (string-prefix? "PATH=" var)
      ;;                         (string-append
      ;;                          "PATH="
      ;;                          #$(file-append (home-emacs-configuration-emacs config) "/bin")
      ;;                          ":"
      ;;                          (substring var 5))
      ;;                         var))


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

