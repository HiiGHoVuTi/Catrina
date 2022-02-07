{ mkDerivation, base, containers, deepseq, haskeline, lib, mtl
, optparse-applicative, parsec, pretty-simple, process, text
, transformers
}:
mkDerivation {
  pname = "catrina";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers deepseq haskeline mtl optparse-applicative parsec
    pretty-simple process text transformers
  ];
  description = "Catrina (rina for short) is a categorical programming language";
  license = lib.licenses.gpl3Only;
}
