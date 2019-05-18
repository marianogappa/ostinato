#!/usr/bin/env bash

# Quit on first error
set -eu

VERSION=$(./version.sh)

# Require envs set
: "${WORKSPACE?WORKSPACE env not set}"
: "${HOME?HOME env not set}"
: "${VERSION?VERSION not set}"

# Clean Ivy Cache
rm -rf "$HOME"/.ivy2/local/org.gappa

cd "$WORKSPACE"/ostinato

# Build binaries/compiled versions
sbt test
sbt clean
sbt publishLocal
sbt fullOptJS

cd -

# Update & push to ostinato-example repo
# TODO use npm in this repo, so this step is unnecessary
cd "$WORKSPACE"/ostinato-example
cp "$WORKSPACE"/ostinato/js/target/scala-2.12/ostinato-opt.js ostinato.js
git pull --rebase
git pull --rebase
git commit -am "Updates library."
git push

cd -

# Updates JS npm package repo
cd "$WORKSPACE"/ostinatojs
cp "$WORKSPACE"/ostinato/js/target/scala-2.12/ostinato-opt.js ostinato.js
jq '.version=env.VERSION' package.json > package.json.tmp && mv package.json.tmp package.json
git pull --rebase
git pull --rebase
git commit -am "Updates library."
git push

cd -
