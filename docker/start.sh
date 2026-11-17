#!/bin/sh
set -eu

relay="${RELAY_URL:-}"
# Only characters that need no escaping in a JavaScript string inside HTML
# and in the sed replacement below.
if [ -n "$relay" ] && ! printf '%s' "$relay" | grep -Eqx 'https?://[A-Za-z0-9._~:/%+=,;@?-]+'; then
    echo "RELAY_URL must be an http(s) URL without quotes, spaces or other special characters: $relay" >&2
    exit 1
fi

sed "s|var configuredRelay = \"\";|var configuredRelay = \"$relay\";|" \
    /opt/tournament/index.html > /home/static/index.html

exec thttpd -D -h 0.0.0.0 -p 3000 -d /home/static -u static -l - -M 60
