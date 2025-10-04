;; This is a sample Guix Home configuration which can help setup your
;; home directory in the same declarative manner as Guix System.
;; For more information, see the Home Configuration section of the manual.
(define-module (guix-home-config)
  ;; #:use-module (home-services pipewire) ; TESTING
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  ;; #:use-module (gnu home services sound)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu system shadow))

(define home-config
  (home-environment
   ;; (packages
   ;;  (list
   ;;   alacritty
   ;;   audacity
   ;;   cable ; optional for pipewire? try it out, never opened it yet
   ;;   deluge
   ;;   emacs
   ;;   fd
   ;;   file
   ;;   git
   ;;   google-chrome-stable
   ;;   htop
   ;;   mednafen
   ;;   openssh-sans-x
   ;;   picard
   ;;   pipewire
   ;;   ranger
   ;;   ripgrep
   ;;   stow
   ;;   tmux
   ;;   vim
   ;;   wireplumber))
   (services
    (list
     (service home-bash-service-type)
     ;; (service home-dbus-service-type) ; required by pipewire service
     (service home-files-service-type
              `((".guile" ,%default-dotguile)
                (".Xdefaults" ,%default-xdefaults)))
     (service home-pipewire-service-type)
     (service home-shepherd-service-type)
     (service home-xdg-configuration-files-service-type
              `(("gdb/gdbinit" ,%default-gdbinit)
                ("nano/nanorc" ,%default-nanorc)))))))

home-config
