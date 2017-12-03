{ mkDerivation, base, JuicyPixels, linear, optparse-applicative
, primitive, random, stdenv, vector
}:
mkDerivation {
  pname = "bar-noise";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base JuicyPixels linear optparse-applicative primitive random
    vector
  ];
  license = stdenv.lib.licenses.mit;
}
