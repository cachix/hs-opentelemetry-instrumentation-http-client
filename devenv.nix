{ pkgs, ... }:

{
  languages.haskell.enable = true;

  packages = [
    pkgs.opentelemetry-collector-contrib

    # Debug Haskell
    pkgs.haskellPackages.eventlog2html
  ];
}
