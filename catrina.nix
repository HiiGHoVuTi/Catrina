{ mkDerivation, base, lib, parsec, text }:
mkDerivation {
  pname = "catrina";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base parsec text ];
  description = "Catrina (rina for short) is a categorical programming language";
  license = lib.licenses.gpl3Only;
}
