# move my keys from my usb drive to my new nixos installation
mkdir /tmp/Private
mount /dev/disk/by-id/usb-PNY_USB_2.0_FD_A0708H32YD34001168-0:0 /tmp/Private
cp -rp /tmp/Private/Private /mnt/"${nix_repo_name}"/dotfiles
umount /tmp/Private
/mnt/nix-config

# make my symlinks
/mnt/nix-config/dotfiles/bin/ghettolinker.sh
