#!/bin/bash

trap "exit" INT TERM ERR
trap "kill 0" EXIT

elm-live \
    --port=8001 \
    --start-page=../index.html \
    src/Main.elm \
    -u \
    -- --output=index.js
