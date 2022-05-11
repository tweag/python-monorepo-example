{ pkgs ? import ./nixpkgs.nix {
  # Force Rosetta on ARM Macs, where vips is unavailable
  localSystem = if builtins.currentSystem == "aarch64-darwin"
                then "x86_64-darwin"
                else builtins.currentSystem; } }:

with pkgs;

mkShell {
  buildInputs = [ nodejs autoconf automake nasm vips libtool libpng pkg-config ];
  shellHook = ''
   PATH="$PWD/node_modules/.bin:$PATH"
  '';
}
