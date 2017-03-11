#!/usr/bin/env bash

set -e

PROJECT_VERSION=$(./version.sh)
PROJECT_SCALA_VERSION=$(./scalaMajorVersion.sh)
ZIP_NAME="ostinato-${PROJECT_VERSION}"
ZIP_PATH="target/${ZIP_NAME}"
DOCKER_TAG=${TAG-latest}

SCRIPT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $SCRIPT_DIR

if [[ ! -f "${ZIP_PATH}.zip" ]]; then
    echo >&2 "Please run 'sbt packArchiveZip' first."
    exit 1
fi

if [[ $(find . -name "${ZIP_NAME}.zip" -mmin -30 | wc -l) -eq 0 ]]; then
    echo >&2 "Please run 'sbt packArchiveZip' again; zip file exists but it was last modified more than a half hour ago. Otherwise hack me."
    exit 1
fi

if [[ "$DOCKER_TAG" == "latest" ]]; then
    echo >&2 "Building docker tag [latest]. For custom tag use e.g.: TAG=v1.0.0 ./docker-build.sh"
fi

find $SCRIPT_DIR -name "${ZIP_NAME}.zip" -exec unzip -o {} -d target/ \;
mkdir -p target/pack/lib
mv -f -v target/${ZIP_NAME}/lib/* "target/pack/lib"

rm target/pack/lib/*_sjs*.jar

echo "Moving ostinato jar out of lib folder so that there are less changes to be added to the docker image"
mv target/pack/lib/ostinato*.jar target/pack/ostinato.jar

echo "Creating the docker image ostinato:${DOCKER_TAG}"
docker build --quiet=false -t ostinato:$DOCKER_TAG .
