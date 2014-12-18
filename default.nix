let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./case-insensitive.nix {}
