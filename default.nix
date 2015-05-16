{ mkDerivation, base, containers, deepseq, free, hoopl, hspec
, hspec-expectations, lens-family-core, linearscan, stdenv
, transformers
}:
mkDerivation {
  pname = "linearscan-hoopl";
  version = "0.4.0.1";
  src = ./.;
  buildDepends = [
    base containers free hoopl linearscan transformers
  ];
  testDepends = [
    base containers deepseq hoopl hspec hspec-expectations
    lens-family-core linearscan transformers
  ];
  homepage = "http://github.com/jwiegley/linearscan-hoopl";
  description = "Makes it easy to use the linearscan register allocator with Hoopl";
  license = stdenv.lib.licenses.bsd3;
}
