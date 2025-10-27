(define-module (jstamant home)
  #:use-module (gnu) ; TODO remove this once the package list is split into other modules (only required for (use-package-modules))
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services sway) ; TODO remove this once moved over to my own sway module
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (jstamant home-services channels)
  #:use-module (jstamant home-services emacs)
  #:use-module (jstamant home-services gaming)
  #:use-module (jstamant home-services sway)
  #:use-module (nongnu packages chrome)
  #:use-module (nongnu packages fonts)
  #:export (home-config))

;; TODO remove this once the package list is split into other modules
(use-package-modules admin audio bittorrent disk file fonts gnome music package-management rust-apps ssh terminals tmux version-control vim)

(define home-config
  (home-environment
    ;; TODO these packages need to be placed into service or organized differently
    (packages
     (list
      ;; Torrent
      deluge

      ;; Terminal emulators
      alacritty

      ;; Settings
      gnome-tweaks

      ;; Command-line utilities
      fd ; to add to emacs?
      file
      htop
      openssh ; TODO add to an ssh.scm home-service file
      ranger
      ripgrep ; to add to emacs
      stow
      tmux

      ;; Version control
      git

      ;; Fonts
      font-awesome-nonfree
      font-dejavu
      font-liberation

      ;; Browsers
      google-chrome-stable

      ;; Multi-media
      audacity
      ;; picard

      ;; Sound - not working yet
      ;; cable ; optional for pipewire? try it out, never opened it yet
      pipewire
      wireplumber

      ;; Editors
      vim))

    (services
     (list
      (simple-service 'jstamant/bash-service
                      home-bash-service-type
                      (home-bash-extension
                        ;; This is the standard Guix Home bash PS1, but in blue
                        ;; PS1='\[\e[1;34m\]\u@\h \w${GUIX_ENVIRONMENT:+ [env]}\$\[\e[0m\] '
                        ;; TODO change this so it's not all escaped, which makes it difficult to read. Will need  to use gexps for this, I think
                        (bashrc (list (plain-file "bash-ps1" "\nPS1='\\[\\e[1;34m\\]\\u@\\h \\w${GUIX_ENVIRONMENT:+ [env]}\\$\\[\\e[0m\\] '")))
                        (aliases
                         '(("ls" . "ls --color=auto")
                           ("l" . "ls -CF")
                           ("ll" . "ls -l")
                           ("la" . "ls -la")
                           ("l1" . "ls -1")
                           ("grep" . "grep --color=auto")
                           ("dl" . "cd ~/downloads")
                           ("dot" . "d ~/.dotfiles")
                           ("sudo" . "sudo ") ; Evaluate aliases preceded by sudo
                           (".." . "cd ..")
                           ("..." . "cd ../..")
                           ("...." . "cd ../../..")
                           ("op" . "open-project.sh")))))
      (service home-dotfiles-service-type
               (home-dotfiles-configuration
                 (directories '("../stow")) ; root of this repo
                 (layout 'stow)))
      (service home-files-service-type
               `((".guile" ,%default-dotguile)
                 (".Xdefaults" ,%default-xdefaults)))

      ;; TODO move nix config to its own module, along with associated dotfiles
      (simple-service 'nix-env-vars-service
                      home-shell-profile-service-type
                      (list
                       (mixed-text-file
                        "nix-env-vars.sh"
                        "source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" "\n"
                        "source /run/current-system/profile/etc/profile.d/nix.sh")))

      ;; TODO move ssh and ssh-agent services to its own file
      (service home-openssh-service-type
               (home-openssh-configuration
                 ;; Automatically add keys to eliminate future passphrase-use
                 (add-keys-to-agent "yes")
                 (hosts
                  (list
                   ;; Host for personal account
                   (openssh-host
                     (name "github.com")
                     (user "git")
                     (host-name "github.com")
                     (identity-file "~/.ssh/id_ed25519"))
                   ;; Host for work account, set git remote to
                   ;; git@github.com-1life:1Life-Workplace-Safety-Solutions/1Life.git
                   (openssh-host
                     (name "gh-1life")
                     (user "git")
                     (host-name "github.com") ; or github.com-1life ??
                     (identity-file "~/.ssh/id_ed25519"))))))

      (service home-shepherd-service-type)
      (service home-ssh-agent-service-type)
      (service home-sway-service-type jstamant-sway-configuration)
      (service home-xdg-configuration-files-service-type
               `(("gdb/gdbinit" ,%default-gdbinit)
                 ("nano/nanorc" ,%default-nanorc)))
      (service home-xdg-user-directories-service-type
               (home-xdg-user-directories-configuration
                 ;; Eliminate some of these folders by pointing them to $HOME
                 (desktop "$HOME/desktop")
                 (download "$HOME/downloads")
                 (templates "$HOME/")
                 (publicshare "$HOME/")
                 (documents "$HOME/")
                 (music "$HOME/music")
                 (pictures "$HOME/images")
                 (videos "$HOME/videos")))
      (service jstamant-channels-service-type)
      (service jstamant-emacs-service-type)
      (service jstamant-gaming-service-type)))))

home-config
