## How I bootstrap customized, per-machine zfs-on-root NixOS installs with one command using this repo.

### My Laptop:
```bash
[root@nixos:~] themelios latitudeE6430 a-schaefers/nix-config
```

### My Workstation:
```bash
[root@nixos:~] themelios hpZ620 a-schaefers/nix-config
```

## The Golden Chain
[themelios.sh](https://github.com/a-schaefers/themelios) bootstraps machine-specific settings from configuration.sh and default.nix in the [hosts/ directory](https://github.com/a-schaefers/nix-config/tree/master/hosts).

[NixOS](https://nixos.org/) with [lib/recimport.nix](https://github.com/a-schaefers/nix-config/blob/master/lib/recimport.nix) auto-imports _everything_ in the [modules/ directory](https://github.com/a-schaefers/nix-config/tree/master/modules).

[dotfiles/emacs.d/init.el](https://github.com/a-schaefers/nix-config/blob/master/dotfiles/.emacs.d/init.el) uses [straight.el](https://github.com/raxod502/straight.el) for reproducible package management and easy use-package integration.

May the source be with you. |—O—|
