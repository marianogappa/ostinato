#!/usr/bin/env bash

set -e

PROJECT_VERSION=$(./version.sh)
PROJECT_SCALA_VERSION=$(./scalaMajorVersion.sh)

NAME="ostinato"
ZIP_NAME="${NAME}-${PROJECT_VERSION}"
ZIP_PATH="target/${ZIP_NAME}"

# Docker image name and tag
IMAGE_NAME=$1
TAG=$2

SCRIPT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $SCRIPT_DIR

if [ -z "$IMAGE_NAME" ] ; then
	cat << 'EOF'

  Usage:  ./docker-build.sh <image-name> <tag>
  e.g.:   ./docker-build.sh ostinato-server test

EOF
	exit 1
fi

if [[ ! -f "${ZIP_PATH}.zip" ]]; then
    echo >&2 "Please run 'sbt packArchiveZip' first."
    exit 1
fi

find $SCRIPT_DIR -name "${ZIP_NAME}.zip" -exec unzip -o {} -d target/ \;
mkdir -p target/pack/lib
mv -f -v target/${ZIP_NAME}/lib/* "target/pack/lib"

#TODO remove sjs files
rm target/pack/lib/*_sjs*.jar

echo "Moving $IMAGE_NAME jar out of lib folder so that there are less changes to be added to the docker image"
mv target/pack/lib/$NAME*.jar target/pack/$NAME.jar


echo "Creating the docker image ${IMAGE_NAME}:${TAG}"
docker build --quiet=false -t $IMAGE_NAME:$TAG .
