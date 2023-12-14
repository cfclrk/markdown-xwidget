#!/bin/bash

tempdir=$(mktemp -d)

echo "Fetching resource bundle from highlightjs.org..."

# Only specify one language because we're just getting this bundle for the CSS.
# All of the CSS files come with this resource bundle. We'll get the javascript
# from a CDN. Downloading too many languages here casuse an error.
curl -s -X POST \
     -H 'Content-Type: application/json' \
     -d '{
  "api": 2,
  "languages": [
    "bash"
  ]
}' \
     https://highlightjs.org/api/download \
     > "$tempdir/out.zip"

echo "Extracting resource bundle to: $tempdir"

unzip -q \
      -d "$tempdir" \
      "$tempdir/out.zip"

d=resources/highlight_css
css_files=$(find "$tempdir/styles/" ! -name "*.min.css")
for f in $css_files; do
    # Parameter expansion to remove prefix pattern
    file_name="${f##*/}"
    cp "$f" "$d/$file_name"
done

echo "Downloading highlight.min.js from CDN"

curl \
    https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/es/highlight.min.js \
    > resources/highlight.min.js

echo "Fetching mermaid js"

curl -s \
     https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js \
     > resources/mermaid.min.js

echo "Fetching MathJax js"

curl -s \
     https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js \
     > resources/tex-mml-chtml.js
