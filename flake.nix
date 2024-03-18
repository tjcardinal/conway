{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      mkDevShell = inputs: pkgs.mkShell {
        buildInputs = inputs;
      };
    in
    {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;

      devShells.${system}.fennel = mkDevShell (with pkgs;[ love fnlfmt fennel-ls ]);
      devShells.${system}.rust = mkDevShell (with pkgs;[ cargo clippy rustc rustfmt ]);
    };
}
