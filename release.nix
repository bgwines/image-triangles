let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project =
            haskellPackagesNew.callPackage ./project.nix { };

          SVGFonts =
            haskellPackagesNew.callPackage ./SVGFonts.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> {inherit config;};

in
  { project = pkgs.haskellPackages.project;
    
  }
