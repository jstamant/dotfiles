(define-module (jstamant home)
  #:use-module (gnu) ; TODO remove this once the package list is split into other modules (only required for (use-package-modules))
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sway)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (jstamant home-services emacs)
  #:use-module (nongnu packages chrome)
  #:export (home-config))

;; TODO remove this once the package list is split into other modules
(use-package-modules admin audio bittorrent disk emulators file music package-management rust-apps ssh terminals tmux version-control vim wm xdisorg)

(define home-config
  (home-environment
    ;; TODO these packages need to be placed into service or organized differently
    (packages
     (list
      ;; Torrent
      deluge

      ;; Terminal emulators
      alacritty

      ;; Command-line utilities
      fd ; to add to emacs?
      file
      htop
      ranger
      ripgrep ; to add to emacs
      stow
      tmux

      ;; Version control
      git

      ;; Browsers
      google-chrome-stable

      ;; Multi-media
      audacity
      ;; picard

      ;; Gaming
      mednafen
      openssh-sans-x

      ;; Sound - not working yet
      ;; cable ; optional for pipewire? try it out, never opened it yet
      pipewire
      wireplumber

      ;; Window-managers / related programs
      rofi
      sway
      waybar

      ;; Editors
      vim))

    (services
     (list
      (service home-bash-service-type)
      (service home-dotfiles-service-type
               (home-dotfiles-configuration
                 (directories '("../files")) ; root of this repo
                 (layout 'stow)))
      (service home-emacs-service-type)
      (service home-files-service-type
               `((".guile" ,%default-dotguile)
                 (".Xdefaults" ,%default-xdefaults)))
      (service home-shepherd-service-type)
      (service home-sway-service-type) ; TODO make custom service and don't install foot
      (service home-xdg-configuration-files-service-type
               `(("gdb/gdbinit" ,%default-gdbinit)
                 ("nano/nanorc" ,%default-nanorc)))))))

home-config
