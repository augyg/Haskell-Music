{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskLib = pkgs.haskell.lib;
  
  f = { mkDerivation, aeson, base, bytestring, lib, matplotlib
      , process, cabal-install, HCodecs, wavy, binary, either, filepath, transformers
      , python3, which 
      }:
      mkDerivation {
        pname = "makeMusic";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring matplotlib process HCodecs which
        #   (haskLib.doJailbreak wavy)
        #   (pkgs.callHackage "riff" ) #binary either filepath transformers
        ];
        librarySystemDepends = [ cabal-install python3 ];
        license = "unknown";
        mainProgram = "makeMusic";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
