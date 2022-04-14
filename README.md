# Zyre bindings for Haskell

This is a haskell package 'zyre2' for using the C library zyre. The package requires
'czmq' and 'zyre' to be installed on the system.

The bindings has support for all zyre features, except the following:

 * Gossip protocol discovery
 * Binding specfic endpoints

## Experimental

The package is currently in an experimental state. The bindings have been tested and validated using the standard zyre examples (minimal.c, chat.c) with 'valgrind', and no memory leaks are found. That stated, there could be issues which have not been uncovered during testing and the library could potentially leak memory or at worst segfault your entire process.

The API may also drastically change, should the current API prove cumbersome to work with.

Tread with care (for now).

## Documentation

The package is thoroughly documented with haddock, which can be found at X or generated using 'cabal haddock' or 'stack haddock'.
