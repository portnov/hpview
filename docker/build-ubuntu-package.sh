#!/bin/bash
set -e

SRC=$(realpath $PWD/..)

docker build -t hpview-ubuntu -f Dockerfile.ubuntu .

docker run --name hpview-ubuntu --rm -v $(pwd)/target:/dst -v $(pwd)/stack:/root/.stack -v $SRC:/src hpview-ubuntu

