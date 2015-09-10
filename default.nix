{ mkDerivation, base, cmdargs, containers, data-default, directory
, filepath, here, hspec, HStringTemplate, language-c, logging
, monad-logger, mtl, pretty, split, stdenv, temporary, text
, transformers
}:
mkDerivation {
  pname = "c2hsc";
  version = "0.7.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default directory filepath HStringTemplate
    language-c logging mtl pretty split temporary text transformers
  ];
  executableHaskellDepends = [
    base cmdargs containers data-default directory filepath
    HStringTemplate language-c logging pretty split temporary text
    transformers
  ];
  testHaskellDepends = [ base here hspec logging monad-logger text ];
  homepage = "https://github.com/jwiegley/c2hsc";
  description = "Convert C API header files to .hsc and .hsc.helper.c files";
  license = stdenv.lib.licenses.bsd3;
}
