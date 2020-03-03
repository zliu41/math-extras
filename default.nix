{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hedgehog, stdenv }:
      mkDerivation {
        pname = "math-extras";
        version = "0.1.0.1";
        src = ./.;
        libraryHaskellDepends = [ base ];
        testHaskellDepends = [ base hedgehog ];
        homepage = "https://github.com/zliu41/math-extras";
        description = "A variety of mathematical utilities";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
