{
  description = "Autolith - a live, self-modifying Common Lisp agent";

  # Autolith pins an exact SBCL (see sbcl.version) and its Quicklisp package
  # set is generated against a matching nixpkgs. Pin that nixpkgs here so the
  # build is reproducible; bump it in lockstep whenever sbcl.version changes.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/f205b5574fd0cb7da5b702a2da51507b7f4fdd1b";

  outputs = { self, nixpkgs }:
    let
      # Autolith's runtime pins an x86_64 SBCL and the package asserts the
      # host platform, so only x86_64-linux is supported.
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      autolith = import ./nix/package.nix {
        inherit pkgs;
        src = self;
      };
    in
    {
      packages.${system} = {
        default = autolith;
        autolith = autolith;
      };

      apps.${system}.default = {
        type = "app";
        program = "${autolith}/bin/autolith";
      };
    };
}
