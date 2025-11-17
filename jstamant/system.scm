(define-module (jstamant system)
  #:use-module (gnu)
  #:use-module (jstamant home)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-service-modules containers cups desktop docker guix networking nix pm ssh shepherd virtualization xorg)

(use-package-modules file gnome-xyz haskell-apps linux package-management scanner tmux version-control vim)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware sof-firmware)) ;; sof-firmeware required for sound on my laptop
 (initrd microcode-initrd)
 (locale "en_CA.utf8")
 (timezone "America/Halifax")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "navy")

 (users (cons* (user-account
                (name "jstamant")
                (comment "Justin St-Amant")
                (group "users")
                (home-directory "/home/jstamant")
                (supplementary-groups '("wheel"
                                        "input" ; required by kmonad
                                        "netdev"
                                        "audio"
                                        "video"
                                        "lp"
                                        "docker")))
               %base-user-accounts))

 ;; System-level packages that are either necessary or convenient for
 ;; when no profile is loaded
 (packages
  (cons* brightnessctl
         file
         git
         gnome-shell-extension-blur-my-shell
         sane-airscan
         sane-backends
         stow
         tmux
         vim
         %base-packages))

 (services
  (append (list
           (service gnome-desktop-service-type)
           (service guix-home-service-type `(("jstamant" ,home-config)))

           (service bluetooth-service-type)

           (service sane-service-type
                    (sane-configuration
                     (backends (list sane-airscan sane-backends))))
           (service cups-service-type
                    (cups-configuration
                      (web-interface? #t)))
           ;; (service dbus-root-service-type)
           ;; The D-Bus clique, from RDE
           ;; (service accountsservice-service-type)
           ;; (service cups-pk-helper-service-type)
           ;; (service colord-service-type)
           ;; (service avahi-service-type) ; TODO move with printing service, might need firewall opened-up on port 5353

           (service nix-service-type)

           (udev-rules-service 'keyboard-configuration kmonad) ; required for users of group "input" to run kmonad

           (service tlp-service-type
                    (tlp-configuration
                     (stop-charge-thresh-bat0 80)
                     (stop-charge-thresh-bat1 80)))

           ;; Setting default brightness, slightly modified from RDE
           (simple-service
            'backlight-set-brightness-on-startup
            shepherd-root-service-type
            (list (shepherd-service
                    (provision '(startup-brightness))
                    (requirement '(term-tty1))
                    (start
                     #~(lambda ()
                         (invoke #$(file-append brightnessctl "/bin/brightnessctl")
                                 "set" (string-append
                                        (number->string 30) "%"))))
                    (one-shot? #t))))
           (udev-rules-service 'backlight brightnessctl)

           ;; Enable Docker containers and virtual machines, pulled from David Wilson's config
           (service containerd-service-type)
           (service docker-service-type)
           (service libvirt-service-type
                    (libvirt-configuration
                     (unix-sock-group "libvirt")
                     (tls-port "16555")))

           (simple-service 'oci-provisioning
                           oci-service-type
                           (oci-extension
                            (containers
                             (list
                              (oci-container-configuration
                               (image "jellyfin/jellyfin")
                               (provision "jellyfin")
                               (network "host")
                               (ports
                                '(("8096" . "8096")))
                               (volumes
                                '("jellyfin-config:/config"
                                  "jellyfin-cache:/cache"
                                  "/home/jstamant/jellyfin:/media")))))))

           (set-xorg-configuration
            (xorg-configuration (keyboard-layout keyboard-layout))))
          (modify-services
           %desktop-services
           (guix-service-type config => (guix-configuration
                                         (inherit config)
                                         (substitute-urls
                                          (append (list "https://substitutes.nonguix.org")
                                                  %default-substitute-urls))
                                         (authorized-keys
                                          (append (list (local-file "./nonguix-signing-key.pub"))
                                                  %default-authorized-guix-keys)))))))

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))

 (initrd-modules (append '("vmd") %base-initrd-modules))

 (swap-devices (list (swap-space
                      (target (uuid
                               "9d6e5abe-13c2-42c3-8ba9-233135bb42fc")))))

 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "A68D-7A12"
                                     'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device (uuid
                                "9cda8336-7fe5-4da7-a011-72284e778109"
                                'ext4))
                       (type "ext4")) %base-file-systems)))
