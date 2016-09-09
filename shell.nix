with import <nixpkgs> {};
haskell.lib.buildStackProject {
  name = "pandoc-light";
  ghc = haskell.packages.ghcjs.ghc;
  buildInputs = [ haskellPackages.hsb2hs ];
}
