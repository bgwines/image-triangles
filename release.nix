let
  nixpkgs = fetchGit {
    url = git://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-18.09";
  };

  config = {
	packageOverrides = pkgs: rec {
	  haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
           SVGFonts 
            = pkgs.haskellPackages.callHackage "SVGFonts" "1.6.0.3" {};

           project
           = haskellPackagesNew.callCabal2nix "image-triangles" ../image-triangles {};

		};
	  };
	};
  };

  pkgs = import nixpkgs {inherit config;};
in
  {
   project = pkgs.haskellPackages.project;
  }
