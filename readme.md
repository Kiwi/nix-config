## What
This repo contains all of my Linux configuration files for everything I do on a computer.
Using this setup, I am able to bootstrap my complete, custom NixOS installations in one-command.
This includes zfs-on-root with auto snapshotting, custom disk array and pool setup,
my operating system services and settings, all of my favorite applications-- _the works!_

## Why
I like to build robust and reproducible, automated systems.
By reducing state we can increase productivity and developer happiness. :)

Let's just call it, "personal computer hygeine."

## How it works
[Themelios](https://github.com/a-schaefers/themelios) is built into a NixOS
[livedisk](https://github.com/a-schaefers/nix-config/blob/master/iso/myrescueiso.nix)
and bootstraps machine-specific installations, using the configuration.sh and default.nix files
within the [hosts/ directory](https://github.com/a-schaefers/nix-config/tree/master/hosts).

[NixOS](https://nixos.org/) with
[lib/recimport.nix](https://github.com/a-schaefers/nix-config/blob/master/lib/recimport.nix)
auto-imports _everything_ in the
[modules/ directory](https://github.com/a-schaefers/nix-config/tree/master/modules). The
corresponding hosts/machine-name/default.nix file determines which modules should
actually be enabled or not.

[dotfiles/.emacs.d/init.el](https://github.com/a-schaefers/nix-config/blob/master/dotfiles/.emacs.d/init.el)
uses [straight.el](https://github.com/raxod502/straight.el) for reproducible Emacs
package management and [use-package](https://github.com/jwiegley/use-package) integration.

May the source be with you. |—O—|

## laptop:
```bash
[root@nixos:~] themelios latitudeE6430 a-schaefers/nix-config
```

## workstation:
```bash
[root@nixos:~] themelios hpZ620 a-schaefers/nix-config
```
