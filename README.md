# Overview

Crepitans is a library and utility for exploring binaries with a scripting interface.  It will expose binary analysis and symbolic execution primitives to users to make precise binary exploration approachable and repeatable.

Some of the high-level design goals include

- Supporting scripting in Scheme (and hopefully Python)
- Supporting a pure query interface via CodeQL
- Providing a simplified Haskell library to perform the same types of exploration as exposed by the scripting interfaces

Note that it would be nice to include support for source languages, too, but at this point the design does not make any particular allowances for non-binary code artifacts.

# Build Instructions

After cloning the repository and changing to the directory containing it:

```
git submodule update --init
pushd deps/softfloat-hs
git submodule update --init
popd
ln -s cabal.project.dist cabal.project
cabal configure -w ghc-8.10.7 pkg:crepitans
cabal build pkg:crepitans

```

# Usage

```
cabal run exe:crepitans -- demos/demo1.scm
```
