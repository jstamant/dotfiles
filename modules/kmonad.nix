{ config, lib, pkgs, ... }:

{
  options.kmonad.enable = lib.mkEnableOption
    "Whether to enable kmonad for user jstamant";

  config = lib.mkIf config.kmonad.enable {
    environment.systemPackages = [ pkgs.kmonad ];

    # Required groups for user access to /dev/input through kmonad
    users.users.jstamant.extraGroups = [ "input" ];

    home-manager.users.jstamant = { pkgs, ... }: {
      home.file.".config/kmonad/config.kbd".text = ''
;; -*- mode: lisp-data -*-
(defcfg
 ;; /dev/input/event3 on Guix SD
 ;; /dev/input/event0 on NixOS
 input (device-file "/dev/input/by-path/platform-i8042-serio-0-event-kbd")
 ;; this is my lenovo keyboard: /dev/input/event7 on Guix SD
 ;; input (device-file "/dev/input/by-id/usb-LiteOn_Lenovo_Traditional_USB_Keyboard-event-kbd")
 output (uinput-sink "kmonad-keyboard")
 fallthrough true
 allow-cmd false)

(defsrc spc)

(defalias ctl_spc (tap-hold-next-release 200 spc lctl))

(deflayer base @ctl_spc)
'';
      systemd.user.services.kmonad = {
        Unit = {
          Description = "Run kmonad as a user daemon";
          After = [ "graphical-session.target" ];
        };
        Service = {
          ExecStart =
            "${pkgs.kmonad}/bin/kmonad %h/.config/kmonad/config.kbd";
          Restart = "on-failure";
          RestartSec = "2s";
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };
    };
  };
}
