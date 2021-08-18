#!/usr/bin/env bash
set -euo pipefail

# Build site
stack build
stack exec site clean
stack exec site build

# Clone folder
ioFolder=$(mktemp -d /tmp/hakyll-blog-github-io.XXXXXX)
git clone "git@github.com:renesat/renesat.github.io.git" "${ioFolder}"

echo "${ioFolder}"
rm -rf "${ioFolder:?}/*"

cp -r ./build/site/* "${ioFolder}/"

cd "${ioFolder}"
git add .
git commit -m "Publish"
git push origin main

rm -rf "${ioFolder}"
