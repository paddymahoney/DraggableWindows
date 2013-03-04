!#/bin/sh
ghc --make XMonad/Layout/DraggableWindows.hs
cabal install
xmonad --recompile
xmonad --restart


