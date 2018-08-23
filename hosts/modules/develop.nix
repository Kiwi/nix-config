{ config, pkgs, ... }:
# all system-wide developer environment related settings
{
  imports = [];

  environment.systemPackages = with pkgs; [
    gitAndTools.gitFull gitAndTools.gitflow
    tmux
  ];

}
