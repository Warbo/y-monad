with rec {
  nix-helpers =
    with {
      src = (import <nixpkgs> { overlays = []; }).fetchgit {
        url    = http://chriswarbo.net/git/nix-helpers.git;
        rev    = "3d23689";
        sha256 = "1wgiwbzy3hsjrd2rs2aajyljpjnh3xzz21j2gj0xkba4aqgcz290";
      };
    };
    import "${src}/overlay.nix";
};
[
  nix-helpers
  (import ./overlay.nix)
]
