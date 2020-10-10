{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = { purjs = ./.; };
  shells = {
    ghc = ["purjs"];
    ghcjs = ["purjs"];
  };
  # shells fail otherwise
  withHoogle = false;
})
