with {
  nixpkgs  = import ./nixpkgs.nix;
  overlays = import ./overlays.nix;
};
{
  inherit ((nixpkgs { inherit overlays; }).y-monad)
    commands
    package
    ;
}
