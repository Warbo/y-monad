with {
  # Use system's nixpkgs to fetch a pinned version
  src = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "d529175";
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  };
};
import "${src}"
