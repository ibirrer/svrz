git checkout -b dist
git clean -xdf
npm install
npm run make
sed -i '/build/d' .gitignore
git add . && git commit -m "prepare dist"
git subtree push --prefix build origin gh-pages
git checkout master
git branch -D dist