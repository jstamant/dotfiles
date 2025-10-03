;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
             (gnu services pm) ; for TLP
             (nongnu packages linux))
(use-service-modules cups desktop networking ssh xorg)

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))
  (locale "en_CA.utf8")
  (timezone "America/Halifax")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "navy")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "jstamant")
                  (comment "Justin St-Amant")
                  (group "users")
                  (home-directory "/home/jstamant")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service gnome-desktop-service-type)
                 (service cups-service-type)
                 (service tlp-service-type
                          (tlp-configuration
                           (stop-charge-thresh-bat0 80)
                           (stop-charge-thresh-bat1 80)))
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services we
           ;; are appending to.
           %desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (initrd-modules (append '("vmd") %base-initrd-modules))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "9d6e5abe-13c2-42c3-8ba9-233135bb42fc")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
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
