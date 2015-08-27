{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, deepseq, free, hoopl, hspec
      , hspec-expectations, lens-family-core, linearscan, QuickCheck
      , stdenv, transformers
      }:
      mkDerivation {
        pname = "linearscan-hoopl";
        version = "0.9.1";
        src = ./.;
        libraryHaskellDepends = [
          base containers free hoopl linearscan QuickCheck transformers
        ];
        testHaskellDepends = [
          base containers deepseq hoopl hspec hspec-expectations
          lens-family-core linearscan QuickCheck transformers
        ];
        homepage = "http://github.com/jwiegley/linearscan-hoopl";
        description = "Makes it easy to use the linearscan register allocator with Hoopl";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
