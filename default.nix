{ mkDerivation, base, containers, exceptions, hlint, hspec
, monad-control, mtl, stdenv, time, transformers, transformers-base
}:
mkDerivation {
  pname = "monad-timing";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers exceptions monad-control mtl time transformers
    transformers-base
  ];
  testHaskellDepends = [ base hlint hspec ];
  homepage = "https://github.com/pikajude/monad-timing";
  description = "Monad transformer for recording timing events";
  license = stdenv.lib.licenses.mit;
}
