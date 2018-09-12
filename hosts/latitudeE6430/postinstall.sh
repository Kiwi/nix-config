mkdir /tmp/Private
mount /dev/sdd /tmp/Private
cp -rp /tmp/Private/Private /mnt/"${nix_repo_name}"/dotfiles
umount /tmp/Private
