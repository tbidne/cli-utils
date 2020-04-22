#!/bin/bash

mkdir ./cli
cd ./cli/

echo "one=sleep 1 && echo 1" > legend.txt
echo "long=sleep 2 && echo long" >> legend.txt
echo "bad=some nonsense" >> legend.txt
echo "both=one,long" >> legend.txt