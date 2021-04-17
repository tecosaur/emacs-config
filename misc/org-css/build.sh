#!/usr/bin/env bash
sassc main.scss main.css

sassc --style compressed main.scss main.min.css
if command -v csso &> /dev/null; then
    csso main.min.css -o main.min.css
fi

ls _*.js | sort -V | xargs cat > main.js
