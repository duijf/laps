let
  # Nixpkgs unstable on 2020-03-14
  rev = "991bbef68351b4aa228f5f763c8d2ded2aeeb84e";
  tarball = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "sha256:1d6byw99i4wjwdz501r6b12i8nifwl86gjd43cjvg8f2d78fazpg";
  };
in
  import tarball
