with {
  # Use system's nixpkgs to fetch a pinned version
  src = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "9dd4dda";
    sha256 = "0a5n1122mwwm2ndlr6y5b8x6mi6mja1dw5widaw9sn323aznr800";
  };
};
import "${src}"
