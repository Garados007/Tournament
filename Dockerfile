FROM ubuntu:latest as builder
RUN apt-get -qq update -y && \
    apt-get -qq upgrade -y && \
    apt-get install wget -y && \
    wget -q https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
        -O elm.gz && \
    gunzip elm.gz && \
    chmod +x elm && \
    mv elm /bin/
WORKDIR /src
COPY ./ui/elm.json /src/
COPY ./ui/src /src/src
RUN mkdir /content && \
    elm make --output /content/index.js src/Main.elm

FROM node:latest as js-compressor
RUN npm install uglify-js --global
WORKDIR /content
COPY --from=builder /content/index.js /content/source.js
RUN uglifyjs \
        source.js \
        --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,unsafe_comps,unsafe' \
        --mangle 'reserved=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9]' \
        --output index.js && \
    rm source.js

FROM alpine:latest
RUN apk add thttpd
RUN adduser -D static
USER static
WORKDIR /home/static
COPY index.html index.html
COPY ui/js/ ./js/
COPY ui/css/ ./css/
COPY --from=js-compressor /content/index.js ./
CMD [ "thttpd", "-D", "-h", "0.0.0.0", "-p", "3000", "-d", "/home/static", "-u", "static", "-l", "-", "-M", "60" ]
