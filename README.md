This is an implementation of an application for Reddit AMA style Q&A sessions.

# Building

## Nix
The easiest way to build this is to download the [Nix package
manager](http://nixos.org/nix/) and use one of the following commands:

* `nix-env --install -f default.nix` - build and install the application, making
  it available from the command line under `modernator-haskell`. Use this to
  test deployments.
* `nix-build default.nix` - build the application, resulting in an executable
  under `./result/bin/modernator-haskell`. Use this to test builds.
* `nix-shell --pure shell.nix` - Enter a shell environment where all compile and
  run time dependencies are installed and available. Use this for active
  development. You can use `cabal build` and `cabal repl` to build and enter an
  interactive REPL in this environment, respectively.

Each of the above commands will automatically download and install all
dependencies required by the project.

If you're running NixOS, some of the Haskell packages may be unavailable in the
latest nixos channels. You'll need to use an updated set of package definitions
to use when building and pass them in when building e.g. `nix-build default.nix
--argstr "nixpkgs=<my_pkg_set>"`

## Cabal
You can also build the project using the traditional Haskell `cabal configure &&
cabal build`, using `cabal install` to make the package available in your
executable paths. However this will not make sure any runtime dependencies are
installed.

# Environment Variables

* `MODERNATOR_PORT` - Specify the port to bind to
* `MODERNATOR_STATE_DIR` - Specify the location of the acid state directory

# Dependencies

* GHC 7.10.2

# Helpful Links
[Servant Documentation](http://haskell-servant.readthedocs.io/en/stable/)

