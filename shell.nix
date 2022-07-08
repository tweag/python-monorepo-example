{ pkgs ? import ./nixpkgs.nix {
  # Force Rosetta on ARM Macs, where vips is unavailable
  localSystem = if builtins.currentSystem == "aarch64-darwin"
                then "x86_64-darwin"
                else builtins.currentSystem; } }:

with pkgs;

mkShell {
  buildInputs = [
    python3
    nodejs-slim # the combination of nodejs-slim and npm makes a more recent version of npm available
    nodePackages.npm
    autoconf
    automake
    nasm
    vips
    libtool
    libpng
    pkg-config
    util-linux # to make lscpu available to gatsby
  ];
  shellHook = ''
   PATH="$PWD/node_modules/.bin:$PATH"
  '';
}
