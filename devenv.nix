{ pkgs, ... }:

{
  languages.haskell.enable = true;

  packages = [
    pkgs.opentelemetry-collector

    # Debug Haskell
    pkgs.haskellPackages.eventlog2html
  ];
}
