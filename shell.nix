{ nixpkgs ? import <nixpkgs> { config = {}; overlays = []; }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, bytestring, containers
      , contravariant, cryptonite, data-default, errors, extra, HUnit
      , memory, MonadRandom, mtl, network, QuickCheck
      , quickcheck-instances, random, random-shuffle, stdenv, stm, tasty
      , tasty-hunit, tasty-quickcheck, time, transformers
      , transformers-compat
      }:
      mkDerivation {
        pname = "kademlia";
        version = "1.1.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring containers contravariant cryptonite extra memory
          MonadRandom mtl network random random-shuffle stm time transformers
        ];
        executableHaskellDepends = [
          base binary bytestring containers data-default extra MonadRandom
          mtl network random random-shuffle transformers transformers-compat
        ];
        testHaskellDepends = [
          base binary bytestring containers data-default errors extra HUnit
          MonadRandom mtl network QuickCheck quickcheck-instances random
          random-shuffle stm tasty tasty-hunit tasty-quickcheck time
          transformers transformers-compat
        ];
        homepage = "https://github.com/serokell/kademlia";
        description = "An implementation of the Kademlia DHT Protocol";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
