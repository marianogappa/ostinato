#!/usr/bin/env bash

# Quit on first error
set -eu

VERSION=$(./version.sh)

echo
echo "Require envs set"

: "${WORKSPACE?WORKSPACE env not set}"
: "${HOME?HOME env not set}"
: "${VERSION?VERSION not set}"

echo
echo "Clean Ivy Cache"

rm -rf "$HOME"/.ivy2/local/org.gappa

cd "$WORKSPACE"/ostinato

echo
echo "Build binaries/compiled versions"

sbt test
sbt clean
sbt publishLocal
sbt fullOptJS

cd -

echo
echo "Update & push to ostinato-example repo"

cd "$WORKSPACE"/ostinato-example
git pull --rebase
git pull --rebase
cp "$WORKSPACE"/ostinato/js/target/scala-2.12/ostinato-opt.js ostinato.js
git commit -am "Updates library."
git push

cd -

echo
echo "Done!"
