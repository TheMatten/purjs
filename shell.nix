{ system ? builtins.currentSystem }:
(import ./. { inherit system; }).shells.ghcjs
