file: opts:
  let
    src =
      let
        bootstrap = import <nixpkgs> {};
        nixpkgs = builtins.fromJSON (builtins.readFile file);
      in
        bootstrap.fetchFromGitHub {
          owner = "NixOS";
          repo  = "nixpkgs";
          inherit (nixpkgs) rev sha256;
        };
  in
    import src opts
