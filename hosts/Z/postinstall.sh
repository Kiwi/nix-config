# get my keys off my usb stick
mkdir /tmp/Private
mount /dev/disk/by-id/usb-PNY_USB_2.0_FD_A0708H32YD34001168-0:0 /tmp/Private
cp -rp /tmp/Private/Private /mnt/"${nix_repo_name}"/dotfiles
umount /tmp/Private

# make my symlinks, own my files, and clone my git repos
cat << EOF | nixos-enter
sudo -u adam /"${nix_repo_name}"/dotfiles/bin/ghettolinker.sh

chown -R adam:users /"${nix_repo_name}"
chown -R adam:users /home/adam

echo "nameserver 8.8.8.8" > /etc/resolv.conf
sudo -u adam /"${nix_repo_name}"/dotfiles/bin/clonerepos.sh
EOF
