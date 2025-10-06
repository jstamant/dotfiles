(define-module (jstamant system)
  #:use-module (gnu)
  #:use-module (jstamant home)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-service-modules cups desktop guix networking pm ssh xorg)

(use-package-modules file package-management tmux version-control vim)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
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
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 ;; System-level packages that are either necessary or convenient for
 ;; when no profile is loaded
 (packages
  (cons* file
         git
         stow
         tmux
         vim
         %base-packages))

 (services
  (append (list
           (service gnome-desktop-service-type)
           (service guix-home-service-type `(("jstamant" ,home-config)))
           (service cups-service-type)
           (service tlp-service-type
                    (tlp-configuration
                     (stop-charge-thresh-bat0 80)
                     (stop-charge-thresh-bat1 80)))
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
