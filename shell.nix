{nixpkgs ? import <nixpkgs> { }, compiler ? "ghc801"}:

let
  inherit (nixpkgs) pkgs;
  # at some point in the future I'd like to know how to pass in "enableLibraryProfiling" here
  ghc = pkgs.haskell.packages.${compiler}.ghcWithHoogle (ps: with ps; [
        ]);
in
nixpkgs.haskell.lib.buildStackProject {
  name = "default-stack-shell";
  buildInputs = with pkgs; [
    git links2 xdg_utils zlib
  ];
  LANG = "en_US.UTF-8";
  inherit ghc;
  # shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
