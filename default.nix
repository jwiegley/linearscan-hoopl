{ mkDerivation, base, containers, deepseq, free, hoopl, hspec
, hspec-expectations, lens-family-core, linearscan, stdenv
, transformers, tardis, QuickCheck
}:
mkDerivation {
  pname = "linearscan-hoopl";
  version = "0.9.0";
  src = ./.;
  buildDepends = [
    base containers free hoopl linearscan transformers tardis
    QuickCheck
  ];
  testDepends = [
    base containers deepseq hoopl hspec hspec-expectations
    lens-family-core linearscan transformers tardis QuickCheck
  ];
  homepage = "http://github.com/jwiegley/linearscan-hoopl";
  description = "Makes it easy to use the linearscan register allocator with Hoopl";
  license = stdenv.lib.licenses.bsd3;
}
