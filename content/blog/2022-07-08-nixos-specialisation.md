---
title: Introduction to NixOS specialisations
shortitle: NixOS specialisations
author: Solène Rapenne
tags: [nix, devops]
description: "How to use the NixOS specialisations to manage different boot environments and when to use it"
---

I often wished to be able to define different boot entries for different uses
of my computer. Be it for separating professional and personal use, testing
kernels or using special hardware. NixOS has a unique feature that solves
this problem in a clever way -- [NixOS specialisations](https://nixos.org/manual/nixos/stable/options.html#opt-specialisation).

To make it simple, a NixOS specialisation is a mechanism to describe
additional boot entries when building your system, with specific
changes applied on top of your non-specialised configuration.

# When do you need specialisations

You may have hardware occasionally connected to your computer, and some
of these devices may require incompatible changes to your day-to-day
configuration. Specialisations can create a new boot entry you can use
when starting your computer with your specific hardware connected. This
is common for people with external GPUs (Graphical Processing Unit),
and the reason why I first used specialisations.

With NixOS, when I need my external GPU, I connect it to my computer
and simply reboot my system, and then I choose the eGPU specialisation
in my boot menu, and it just works. My boot menu looks like the following:

![Picture showing a GRUB boot menu with many entries including egpu-with-external display and egpu-with-laptop-display](2022-nixos-specialisation-images/grub-boot-menu-screenshot.png)

You can also define a specialisation which will boot into a different kernel,
giving you a safe opportunity to try a new version while keeping a
fallback environment with the regular kernel.

We can push the idea further by using a single computer for professional
and personal use. Specialisations can have their own users, services,
packages and requirements. This would create a hard separation without
using multiple operating systems. However, by default, such a setup would be
more practical than secure. While your users would only exist in
one specialisation at a time, both users data are stored on the same
partition, so one user could be exploited by an attacker to reach
the other user's data. In a follow-up blog post, I will describe a secure
setup using multiple encrypted partitions with different passphrases,
all managed using specialisations with a single NixOS configuration.

# How to use specialisations

As an example, we will create two specialisations, one having the user
Chani using the desktop environment Plasma, and the other with the user
Paul using the desktop environment Gnome. Auto login at boot will be
set for both users in their own specialisations. Our user Paul will need
an extra system-wide package, for example `dune-release`. Specialisations
can use any argument that would work in the [top-level configuration](https://nixos.org/manual/nixos/stable/options.html),
so we are not limited in terms of what can be changed.

If you want to try, add the following code to your `configuration.nix`
file.

```nix
specialisation = {
  chani.configuration = {
    system.nixos.tags = [ "chani" ];
    services.xserver.desktopManager.plasma5.enable = true;
    users.users.chani = {
      isNormalUser = true;
      uid = 1001;
      extraGroups = [ "networkmanager" "video" ];
    };
    services.xserver.displayManager.autoLogin = {
      enable = true;
      user = "chani";
    };
  };

  paul.configuration = {
    system.nixos.tags = [ "paul" ];
    services.xserver.desktopManager.gnome.enable = true;
    users.users.paul = {
      isNormalUser = true;
      uid = 1002;
      extraGroups = [ "networkmanager" "video" ];
    };
    services.xserver.displayManager.autoLogin = {
      enable = true;
      user = "paul";
    };
    environment.systemPackages = with pkgs; [
      dune-release
    ];
  };
};
```

After applying the changes, run `nix-rebuild boot` as root. Upon reboot,
in the GRUB menu, you will notice a two extra boot entries named "chani"
and "paul" just above the last boot entry for your non-specialised system.

Rebuilding the system will also create scripts to switch from a
configuration to another, specialisations are no exception.

Run `/nix/var/nix/profiles/system/specialisation/chani/bin/switch-to-configuration switch`
to switch to the `chani` specialisation.

When using the switch scripts, keep in mind that you may not have exactly
the same environment as if you rebooted into the specialisation as some
changes may be only applied on boot.

# Conclusion

Specialisations are a perfect solution to easily manage multiple boot
entries with different configurations. It is the way to go when
experimenting with your system, or when you occasionally need specific
changes to your regular system.
