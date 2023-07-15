{
  description = "SMLpg";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, devenv, nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
      });
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor."${system}";
          inherit (pkgs);
        in
        {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              ({ pkgs, lib, ... }: {
                packages = [ pkgs.millet pkgs.polyml pkgs.smlfmt pkgs.gnumake pkgs.gcc ];

                services.postgres = {
                  package = pkgs.postgresql_15.withPackages (p: [ ]);
                  enable = true;
                  initialDatabases = [ { name = "socket"; } ];
                  port = 5432;
                  listen_addresses = "127.0.0.1";
                  initialScript = ''
                  CREATE USER admin SUPERUSER;
                  ALTER USER admin PASSWORD 'admin';
                  '';
                };
              })
            ];
          };
        });
    };
}
