#!/bin/sh

set -e

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

git config --global user.email "michael.rustler@kompetenz-wasser.de"
git config --global user.name "Michael Rustler"

git clone -b gh-pages \
https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git \
book-output
cd book-output
git rm -rf *
cp -r ../docs/tutorial/* ./tutorial/
git add --all *
git commit -m "Update the bookdown tutorial" || true
git push -q origin gh-pages
