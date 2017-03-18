#!/usr/bin/env bash
sbt doc && rm -rf docs/* && cp -rf jvm/target/scala-2.12/api/* docs/
