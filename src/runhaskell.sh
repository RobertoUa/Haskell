#!/bin/sh
echo "Compiling... "
time ghc --make -O2 -Wall Main.hs
echo "Successfully compiled"
echo "Running... "
time ./Main.exe 
echo "Successfully executed "
read -p "Press [Enter] key to exit..."
