let
  # Nixpkgs unstable on 2020-03-14
  rev = "991bbef68351b4aa228f5f763c8d2ded2aeeb84e";
  tarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:0wq4xqm0qliphc11sj08mp8586r039l6v1x1a2cpvb66sh36a54w";
  };
in
  import tarball
