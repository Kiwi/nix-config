{ config, pkgs, lib, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in
{
options.modules.desktop.developer.enable = mkEnableOption "modules.desktop.developer";
config = mkIf config.modules.desktop.developer.enable {

environment.systemPackages = with pkgs; [
# misc
gitAndTools.gitFull gitAndTools.gitflow

# sh / bash
shellcheck

# common lisp
sbcl lispPackages.quicklisp

# clojure
clojure leiningen

# dev-ops
awscli ansible

# graphics
gimp krita inkscape darktable

# word processing
pandoc aspell libreoffice-fresh texlive.combined.scheme-small
];

};
}
