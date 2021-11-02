{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  buildInputs = [ nodejs autoconf automake nasm libtool libpng pkg-config ];
}
