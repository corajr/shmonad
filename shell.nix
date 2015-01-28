# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

with haskellPackages; cabal.mkDerivation (self: {
  pname = "shmonad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ filepath free hspec QuickCheck temporary text ];
  testDepends = [ hspec QuickCheck temporary text ];
buildTools = [ cabalInstall_1_20_0_6 ];
shellHook =
''
e() { ~/.nix-profile/Applications/Emacs.app/Contents/MacOS/emacs $@ & }
'';
  meta = {
    license = self.stdenv.lib.licenses.bsd2;
    platforms = self.ghc.meta.platforms;
  };
})
