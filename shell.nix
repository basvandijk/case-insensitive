let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      caseInsensitive = self.callPackage ./case-insensitive.nix {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.caseInsensitive.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.criterion
       ] ++ hs.caseInsensitive.propagatedNativeBuildInputs
         ++ hs.caseInsensitive.nativeBuildInputs)))
     ];
   }
