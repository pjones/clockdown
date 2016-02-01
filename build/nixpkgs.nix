with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "xmonadrc";

  buildInputs = [
    # GHC:
    haskell.packages.lts-4_2.ghc

    # Non-Haskell Dependencies:
    ncurses
  ];

  # Work around a bug in GHC:
  # https://ghc.haskell.org/trac/ghc/ticket/11042
  shellHook = ''
    export LD_LIBRARY_PATH=${ncurses}/lib
  '';
}
