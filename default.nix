{ cabal, cmdargs, filepath, HStringTemplate, languageC, mtl, split
, transformers, dataDefault, hspec, logging, monadLogger, temporary
, here
}:

cabal.mkDerivation (self: {
  pname = "c2hsc";
  version = "0.6.5";
  sha256 = "0c5hzi4nw9n3ir17swbwymkymnpiw958z8r2hw6656ijwqkxvzgd";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    cmdargs filepath HStringTemplate languageC mtl split transformers
    dataDefault
  ];
  testDepends = [
    here hspec logging monadLogger temporary
  ];
  meta = {
    homepage = "https://github.com/jwiegley/c2hsc";
    description = "Convert C API header files to .hsc and .hsc.helper.c files";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
