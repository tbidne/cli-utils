#!/bin/bash

mkdir ./git-fs
cd ./git-fs/

# User email and name needs to be set or git commands needed for test
# will fail, but we don't want to screw up anyone's settings, so
# only do this if CI env var is set
if [ -n "$CI" ]
then
  git config --global user.email "you@example.com"
  git config --global user.name "CI"
fi

git init
echo "testing" > foo.txt
git add foo.txt
git commit -m "testing"

for i in {1..100}
do
  git branch "merged_$i"
done

git checkout -b "unm_1"
echo "change" > change.txt
git add change.txt
git commit -m "change"
for i in {2..50}
do
  git checkout -b "unm_$i"
done