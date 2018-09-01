## Bootstrap a custom zfs-on-root NixOS install with custom settings in one command.
I bootstrap my entire Linux world from my zfs-on-root setups to my packages / config files / Emacs settings-- by typing only one command.

All of which is in a fully reproduceable manner down the hash check. The key to doing this is using NixOS and straight.el for Emacs.

I wrote a bash script called "Themelios" and it bootstraps the whole setup per machine by typing only one command from a live install disk.

For example, the way I bootstrap my laptop from zero to 100 in -lt 6 seconds is by typing,

    themelios latitudeE6430 a-schaefers/nix-config

For more information check out themelios. https://github.com/a-schaefers/themelios
