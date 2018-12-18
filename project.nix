{ mkDerivation, base, colour, diagrams-lib, diagrams-svg, hip
, parallel, random, stdenv
}:
mkDerivation {
  pname = "image-triangles";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base colour diagrams-lib diagrams-svg hip parallel random
  ];
  license = stdenv.lib.licenses.bsd3;
}
