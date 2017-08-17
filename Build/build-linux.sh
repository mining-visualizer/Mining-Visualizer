#!/bin/bash

cd ..

# lazbuild --build-all --build-mode=Release MiningVisualizer.lpi

# if [ $? -ge 1 ] ; then
#    echo "."
#    echo "."
#    echo "."
#    echo "compilation error"
#    echo "."
#    echo "."
#    echo "."
#    exit 1
# fi

rm -r Release/MiningVisualizer/*

cp MiningVisualizer Release/MiningVisualizer
res=$?

cp -R WebApp Release/MiningVisualizer
res=$((res + $?))

cp -R SSL-Certs Release/MiningVisualizer
res=$((res + $?))


tar -czf Release/MiningVisualizer-vxxx-linux.tar.gz  --directory=Release MiningVisualizer
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
