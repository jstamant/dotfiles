(define-module (jstamant home)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (jstamant home-services emacs)
  #:export (home-config))

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
     (service home-emacs-service-type)
     (service home-files-service-type
              `((".guile" ,%default-dotguile)
                (".Xdefaults" ,%default-xdefaults)))
     (service home-shepherd-service-type)
     (service home-xdg-configuration-files-service-type
              `(("gdb/gdbinit" ,%default-gdbinit)
                ("nano/nanorc" ,%default-nanorc)))))))

home-config
