{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false, inNixShell ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, deepseq, hedgehog, hspec, lib, mtl
      , process, tasty
      }:
      mkDerivation {
        pname = "GHCIInterface";
        version = "0.3.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base deepseq mtl process ];
        testHaskellDepends = [ base hedgehog hspec tasty ];
        homepage = "https://github.com/CubOfJudahsLion/tm-ghci#readme";
        description = "Interface module between GHCi and TeXmacs";
        license = lib.meta.getLicenseFromSpdxId "GPL-3.0-or-later";
        mainProgram = "GHCIInterface";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if inNixShell then drv.env else drv
