#!/bin/sh
docker run \
    --workdir=/src/my-rpcq \
    --entrypoint=sbcl \
    -v $PWD:/src/my-rpcq \
    -e LANG='C.UTF-8' \
    rigetti/rpcq:latest \
    --script /src/my-rpcq/update_python_spec.lisp
