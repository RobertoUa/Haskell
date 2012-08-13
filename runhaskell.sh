#!/bin/sh
echo "Compiling... "
cd E:/Workspace/Haskell
time cabal build
echo "Successfully compiled"
echo "Running... "
cd E:/Workspace/Haskell/dist/build/Haskell
time ./Haskell.exe
echo "Successfully executed "
read -p "Press [Enter] key to exit..."
