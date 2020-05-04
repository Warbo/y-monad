{ attrsToDirs', haskellPackages, lib, run, spaces ? lib.range 1 9, withDeps,
  writeScript }:

with builtins;
with lib;
with rec {
  labels = map (n: "l${toString n}") spaces;

  deps = extra: haskellPackages.ghcWithPackages (h: [
    h.aeson h.polysemy h.process-extras
  ] ++ map (p: getAttr p h) extra);

  mkScript = name: writeScript "haskell-shortcut-${name}.hs" ''
    module Main where
    import Yabai
    main = mkMain ${name}
  '';

  compile = extra: n: main: run {
    name   = "haskell-shortcut-${n}";
    paths  = [ (deps extra) ];
    vars   = { inherit main; };
    script = ''
      #!/usr/bin/env bash
      set -e
      cp "${./Yabai.hs}" Yabai.hs
      sed -e 's/undefined -- LABELS_GO_HERE/${toJSON labels}/g' -i Yabai.hs
      cp "$main" Main.hs
      ghc --make Main.hs -o "$out"
    '';
  };

  tests = run {
    name   = "YabaiTests";
    vars   = {
      tests = compile
        [ "lens" "QuickCheck" "tasty" "tasty-quickcheck" ]
        "tests"
        ./YabaiTests.hs;
    };
    script = ''"$tests" && mkdir "$out"'';
  };

  haskellShortcut = name: withDeps [ tests ] (compile [] name (mkScript name));

  haskellCommands = genAttrs [
    "currentDisplay"
    "currentSpace"
    "currentWindow"
    "displayCount"
    "displayNext"
    "displayPrev"
    "focusHereEnv"
    "labelSpaces"
    "nextWindow"
    "moveWindowNext"
    "moveWindowPrev"
    "prevWindow"
  ] haskellShortcut;
};
rec {
  commands = haskellCommands;
  package  = attrsToDirs' "y-monad" { bin = commands; };
}
