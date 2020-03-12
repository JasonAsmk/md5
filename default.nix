{ mkDerivation, base, bytestring, data-serializer, hspec, mtl
, stdenv, vector
}:
mkDerivation {
  pname = "md5";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring data-serializer mtl vector
  ];
  executableHaskellDepends = [ base bytestring vector ];
  testHaskellDepends = [
    base bytestring data-serializer hspec mtl vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
