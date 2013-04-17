This library implements enumerator/iteratee style I/O, as described at
http://okmij.org/ftp/Haskell/Iteratee/

INSTALLATION INSTRUCTIONS:

This library uses the Hackage/Cabal build system.  You will need a working
Haskell compiler and appropriate build system.  This is most easily met
by installing the Haskell Platform.  The following command will install
the library:

cabal install iteratee

This library is pure Haskell, and should install on any system with a suitable
Haskell compiler with no extra steps required.  In particular, POSIX-compatible,
Mac OSX, and Windows should all be supported.

INSTALLATION OPTIONS:

This library supports the following cabal flags:
  splitBase (default enabled): use the split-up base package.

  buildTests (default disabled): build a test executable.

NOTES:

 -The Data.Iteratee.IO.Posix module is only available on Posix systems.

 -The Data.Iteratee.IO.Windows module is currently a stub.  Currently only the
standard Handle interface is available on Windows.

