{ mkDerivation, base, containers, deepseq, generic-lens, haskeline
, lib, microlens, mtl, optparse-applicative, parsec, pretty-simple
, process, recursion-schemes, text, transformers
}:
mkDerivation {
  pname = "catrina";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers deepseq generic-lens haskeline microlens mtl
    optparse-applicative parsec pretty-simple process recursion-schemes
    text transformers
  ];
  description = "Catrina (rina for short) is a categorical programming language";
  license = lib.licenses.gpl3Only;
}
