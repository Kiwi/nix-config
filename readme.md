## laptop:
```bash
[root@nixos:~] themelios latitudeE6430 a-schaefers/nix-config
```

## workstation:
```bash
[root@nixos:~] themelios hpZ620 a-schaefers/nix-config
```

## How this repo works
[Themelios](https://github.com/a-schaefers/themelios) is built into a NixOS
[livedisk](https://github.com/a-schaefers/nix-config/blob/master/iso/myrescueiso.nix)
and bootstraps machine-specific installations, using the configuration.sh and default.nix files
within the [hosts/ directory](https://github.com/a-schaefers/nix-config/tree/master/hosts).

[NixOS](https://nixos.org/) with
[lib/recimport.nix](https://github.com/a-schaefers/nix-config/blob/master/lib/recimport.nix)
auto-imports _everything_ in the
[modules/ directory](https://github.com/a-schaefers/nix-config/tree/master/modules) and
the hosts/ default.nix file determines which modules should actually be enabled or not.

[dotfiles/.emacs.d/init.el](https://github.com/a-schaefers/nix-config/blob/master/dotfiles/.emacs.d/init.el)
uses [straight.el](https://github.com/raxod502/straight.el) for reproducible Emacs
package management and [use-package](https://github.com/jwiegley/use-package) integration.

May the source be with you. |—O—|
