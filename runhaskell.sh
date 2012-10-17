#!/bin/sh
echo "Compiling... "
cd E:/Workspace/Haskell
time cabal build
echo "Successfully compiled"
echo "Running... "
cd E:/Workspace/Haskell/dist/build/Haskell
mv -f ./Haskell.exe ./run.exe
time  ./run.exe
mv -f ./run.exe ./old.exe
echo "renamed "
read -p "Press [Enter] key to exit..."
