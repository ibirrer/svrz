#!/usr/bin/env bash

set -e

git checkout -b dist
git clean -xdf
npm install
npm run dist
sed -i '/dist/d' .gitignore
git add . && git commit -m "prepare dist"
git subtree split --prefix dist -b gh-pages
git push -f origin gh-pages:gh-pages
git branch -D gh-pages
git checkout master
git branch -D dist