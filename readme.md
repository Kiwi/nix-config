## How I bootstrap customized, per-machine zfs-on-root NixOS installs with one command.

### Laptop:
```bash
[root@nixos:~] themelios latitudeE6430 a-schaefers/nix-config
```

### Workstation:
```bash
[root@nixos:~] themelios hpZ620 a-schaefers/nix-config
```

## The Golden Chain - A reproducible system.
[themelios.sh](https://github.com/a-schaefers/themelios) >
   bootstraps a machine-specific configuration.sh and default.nix from the [hosts/ directory](https://github.com/a-schaefers/nix-config/tree/master/hosts).

[NixOS](https://nixos.org/) with [lib/recimport.nix](https://github.com/a-schaefers/nix-config/blob/master/lib/recimport.nix) >
    auto-imports _everything_ in the [modules/ directory](https://github.com/a-schaefers/nix-config/tree/master/modules).

[Emacs](https://www.gnu.org/software/emacs/) uses [straight.el](https://github.com/raxod502/straight.el) for reproducible package management and easy use-package integration. See [dotfiles/emacs.d/init.el](https://github.com/a-schaefers/nix-config/blob/master/dotfiles/.emacs.d/init.el)

May the source be with you. |—O—|
