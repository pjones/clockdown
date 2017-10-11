{ mkDerivation, async, base, byline, colour, containers, mtl
, stdenv, tasty, tasty-hunit, text, time, transformers, vty
}:
mkDerivation {
  pname = "clockdown";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base byline colour containers mtl text time transformers vty
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  license = stdenv.lib.licenses.bsd2;
}
