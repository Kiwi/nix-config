{ config, pkgs, lib, ... }:
with lib;
let
# myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});

# create my symlinks
adamSymlinks = pkgs.writeScriptBin "mynixos-symlinks" ''
#!${pkgs.stdenv.shell}
ln -sf /nix-config/dotfiles/.bash_profile ~/.bash_profile
ln -sf /nix-config/dotfiles/.bashrc ~/.bashrc
ln -sf /nix-config/dotfiles/.inputrc ~/.inputrc
ln -sf /nix-config/dotfiles/.gitconfig ~/.gitconfig
ln -sf /nix-config/dotfiles/.mailcap ~/.mailcap
ln -sf /nix-config/dotfiles/.Xmodmap ~/.Xmodmap

mkdir ~/.emacs.d
ln -sfn /nix-config/dotfiles/.emacs.d/lisp.d ~/.emacs.d/lisp.d
ln -sf /nix-config/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el

mkdir -p ~/.config/{mpv,mimi,bin}
ln -sf /nix-config/dotfiles/.config/mpv/mpv.conf ~/.config/mpv/mpv.conf
ln -sf /nix-config/dotfiles/.config/mimi/mime.conf ~/.config/mimi/mime.conf
ln -sf /nix-config/dotfiles/bin/emacsmail ~/bin/emacsmail
ln -sf /nix-config/dotfiles/bin/xdg-open ~/bin/xdg-open
ln -sf /nix-config/dotfiles/bin/nextmonitor.sh ~/bin/nextmonitor.sh
'';

# clone my repos script
cloneRepos = pkgs.writeScriptBin "mynixos-cloneRepos" ''
#!${pkgs.stdenv.shell}
mkdir ~/repos/
cd ~/repos/
git clone git@github.com:cooslug/cooslug.github.io.git
git clone git@github.com:a-schaefers/themelios.git
git clone git@github.com:a-schaefers/grubbe-mkconfig.git
git clone git@github.com:apoptosis/episteme.git
git clone git@github.com:a-schaefers/a-schaefers.github.io.git
git clone git@github.com:TemptorSent/Funtools.git
git clone git@github.com:bbatsov/prelude.git
'';

in
{
options.modules.desktop.enable = mkEnableOption "Desktop Profile";
config = mkIf config.modules.desktop.enable {

# splash screen.
boot.plymouth.enable = true;

# setup users
users.users.adam =
{ isNormalUser = true;
home = "/home/adam";
createHome = true;
extraGroups = [ "wheel" "disk" "audio" "video" "systemd-journal"
"networkmanager" "libvirtd" ];
};
security.sudo.wheelNeedsPassword = false;

# misc services I don't really use

# services.samba.enable = true;
# services.locate.enable = true;
# services.printing.enable = true;
# services.avahi.enable = true;
# services.avahi.nssmdns = true;

services.acpid.enable = true;
services.smartd.enable = true;

# pulseaudio
sound.enable = true;
hardware.pulseaudio.enable = true;

# mesa
hardware.opengl = {
driSupport = true;
driSupport32Bit = true;
};

# Xorg, Slim, Exwm
services.xserver = {
enable = true;
layout = "us";
useGlamor = true;
displayManager.slim.enable = true;
displayManager.slim.autoLogin = false;
displayManager.slim.defaultUser = "adam";
displayManager.sessionCommands = ''
${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
${pkgs.xlibs.xset}/bin/xset r rate 250 50
${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
${pkgs.numlockx}/bin/numlockx
${pkgs.dunst}/bin/dunst &
'';

# desktopManager = {
# xterm.enable = false;
# default = "emacs";
# session = [ {
# manage = "desktop";
# name = "emacs";
# start = ''
# ${myEmacs}/bin/emacs &
# waitPID=$!
# '';}];};

# testing this
desktopManager = {
default = "none";
xterm.enable = false;
};
windowManager.exwm.enable = true;
windowManager.exwm.enableDefaultConfig = false;
windowManager.exwm.extraPackages = epkgs: [
# epkgs.emms
# epkgs.magit
# ...
];

};

environment.sessionVariables = {
EDITOR = "emacsclient";
VISUAL = "emacsclient";
_JAVA_AWT_WM_NONREPARENTING = "1";
};

# some basic xft fonts
fonts.fonts = with pkgs; [
dejavu_fonts
source-code-pro ];

environment.systemPackages = with pkgs; [
# Emacs and related things.
# myEmacs
emacs
cloneRepos
adamSymlinks
gitAndTools.gitFull gitAndTools.gitflow
tmux
shellcheck
poppler_utils poppler_gi libpng12 zlib
(python36.withPackages(ps: with ps; [ certifi ]))
gnutls gnupg gnupg1compat

# desktop support applications
glxinfo
wmctrl
scrot
xorg.xmodmap xorg.xev xorg.xrdb xorg.xset xorg.xsetroot
numlockx
xclip xsel
libnotify dunst

# desktop apps
pandoc
firefox qbittorrent mpv pavucontrol
gimp kdenlive darktable krita inkscape

# security
openvpn
];

programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
};
}
