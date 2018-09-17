# get my keys off my usb stick
mkdir /tmp/Private
mount /dev/disk/by-id/usb-PNY_USB_2.0_FD_A0708H32YD34001168-0:0 /tmp/Private
cp -rp /tmp/Private/Private /mnt/"${nix_repo_name}"/dotfiles
umount /tmp/Private

# make my symlinks
/mnt/nix-config/dotfiles/bin/ghettolinker.sh

# own my files
cat << EOF | nixos-enter
chown -R adam:users /nix-config
chown -R adam:users /home/adam
EOF
