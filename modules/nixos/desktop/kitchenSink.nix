{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.kitchenSink.enable = mkEnableOption "modules.desktop.kitchenSink";
config = mkIf config.modules.desktop.kitchenSink.enable {

services.samba.enable = true;
services.locate.enable = true;
services.printing.enable = true;
services.avahi.enable = true;
services.avahi.nssmdns = true;

};
}
