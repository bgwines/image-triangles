let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev    = "50f41ea2fcf86def32799f75577a4fe5cfd1132e";
    sha256 = "1q0bxl5nxx1kabqvyzkdw91c5dnwpi2rwsgs5jdmnj7f0qqgdxh8";
  };

  config = {
	packageOverrides = pkgs: rec {
	  haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
           SVGFonts 
            = pkgs.haskellPackages.callHackage "SVGFonts" "1.6.0.3" {};

           project
            = haskellPackagesNew.callCabal2nix "image-triangles" ../image-triangles  {};
		};
	  };
	};
  };
  pkgs = import nixpkgs {inherit config;};
in
  { 
   project = pkgs.haskellPackages.project;
}
