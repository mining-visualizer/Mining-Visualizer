#!/bin/bash

cd ..

lazbuild --build-all --build-mode=Release MiningVisualizer.lpi

if [ $? -ge 1 ] ; then
   echo "."
   echo "."
   echo "."
   echo "compilation error"
   echo "."
   echo "."
   echo "."
   exit 1
fi

cp MiningVisualizer Release/MiningVisualizer.app/Contents/MacOS
res=$?

rm -r Release/MiningVisualizer.app/Contents/Resources/*

cp -R WebApp Release/MiningVisualizer.app/Contents/Resources
res=$((res + $?))

cp -R GeekTool Release/MiningVisualizer.app/Contents/Resources
res=$((res + $?))

cp -R SSL-Certs Release/MiningVisualizer.app/Contents/Resources
res=$((res + $?))


tar -czf Release/MiningVisualizer-vxxx-osx.tar.gz  --directory=Release MiningVisualizer.app
res=$((res + $?))

if [ "$res" -ge 1 ] ; then
   echo "."
   echo "."
   echo "."
   echo "ERRORS WERE ENCOUNTERED!!!"
   echo "."
   echo "."
   echo "."
   exit 1
fi

echo "."
echo "."
echo "."
echo "script completed successfully"
echo "."
echo "."
echo "."
