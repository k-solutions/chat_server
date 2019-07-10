{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, containers, hpack
      , network, stdenv, stm
      }:
      mkDerivation {
        pname = "NetServer";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base bytestring containers network stm
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          async base bytestring containers network stm
        ];
        testHaskellDepends = [
          async base bytestring containers network stm
        ];
        preConfigure = "hpack";
        homepage = "https://github.com/githubuser/NetServer#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
