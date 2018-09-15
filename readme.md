## What
This repo contains all of my Linux configuration files for everything I do on a computer.
Using this setup, I am able to bootstrap complete, custom NixOS installations with one-command.
This includes zfs-on-root with custom disk arrays and automatic pool setup,
 my system services and settings, all of my favorite applications-- _the works!_

## Why
I like to build robust and reproducible, automated systems.
By reducing state we can increase productivity and developer happiness. :)

Let's just call it, "personal computer hygeine."

## How it all works
1. [Themelios](https://github.com/a-schaefers/themelios) is built into a NixOS
[livedisk](https://github.com/a-schaefers/nix-config/blob/master/iso/myrescueiso.nix)
and bootstraps machine-specific installations, using the corresponding machine-name/configuration.sh
and machine-name/default.nix files from within the
[hosts/ directory](https://github.com/a-schaefers/nix-config/tree/master/hosts).

2. [NixOS](https://nixos.org/) with
[lib/recimport.nix](https://github.com/a-schaefers/nix-config/blob/master/lib/recimport.nix)
auto-imports _everything_ in the
[modules/ directory](https://github.com/a-schaefers/nix-config/tree/master/modules). The
corresponding hosts/machine-name/default.nix file determines which modules should be enabled.

3. I use Emacs for nearly everything I do, it is the central hub of my workflow. The following are
just a few of my favorite Emacs packages that I use daily:
  - exwm (window manager)
  - gnus (email)
  - erc (irc)
  - emms (media player)
  - pdf-tools (pdf viewer)
  - image-mode (image viewer)
  - dired (file manager)
  - eww (browser)
  - org-mode (todo lists, planning and time-management)
  - shell and ansi-term (terminals)
  - [And much more...](https://github.com/a-schaefers/nix-config/tree/master/dotfiles/.emacs.d/lisp.d)

I have made an exception for Emacs, instead of using the nix built-in package manager, in my
[dotfiles/.emacs.d/init.el](https://github.com/a-schaefers/nix-config/blob/master/dotfiles/.emacs.d/init.el)
I use [straight.el](https://github.com/raxod502/straight.el), which
is a "Next-generation, purely functional package manager for the Emacs hacker." I appreciate Straight's
[use-package](https://github.com/jwiegley/use-package) integration and prefer doing all Emacs
configuration and package management in elisp.

## How I bootstrap my complete, custom NixOS installations in one-command:
- My laptop:
```bash
[root@nixos:~] themelios adamant a-schaefers/nix-config
```

- My workstation:
```bash
[root@nixos:~] themelios Z a-schaefers/nix-config
```

## Credits
- Everyone in #nixos and #emacs on freenode, for always helping me out.
- [ldlework's dotfiles](https://github.com/dustinlacewell/dotfiles), which helped inspire my nix files and the themelios installer.
- Various Emacs starter-kits, technomancy, purcell, prelude, spacemacs, the Emacs wiki, etc. I've taken bits and pieces from everywhere by now.

__"Thanks."__
