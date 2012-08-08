#!/bin/sh
echo "Compiling... "
time ghc Main.hs -O -Wall -fglasgow-exts
echo "Successfully compiled"
read -p "Press [Enter] key to con..."
echo "Running... "
time ./Main.exe 
echo "Successfully executed "
read -p "Press [Enter] key to exit..."
