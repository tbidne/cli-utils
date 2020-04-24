#!/bin/bash

mkdir ./git-ff
cd ./git-ff/

# User email and name needs to be set or git commands needed for test
# will fail, but we don't want to screw up anyone's settings, so
# only do this if CI env var is set
if [ -n "$CI" ]
then
  git config --global user.email "you@example.com"
  git config --global user.name "CI"
fi

git init

# setup master
echo "initial" > foo.txt
git add foo.txt
git commit -m "initial commit"

git checkout -b can-ff-one
git checkout -b can-ff-two

git checkout -b cannot-ff
echo "conflict" > conflict.txt
git add conflict.txt
git commit -m "conflict"

git checkout master
echo "new change" >> foo.txt
git add foo.txt
git commit -m "new change"