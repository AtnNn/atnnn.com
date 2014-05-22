#!/bin/bash

set -eu

here=$(dirname "$0")

for ttf in "$here"/*.ttf; do
    ttf=${ttf#$here/}
    name=${ttf%.ttf}
    echo Processing $name >&2
    test -e $name.eot
    test -e $name.svg
    test -e $name.woff
    tags=$(echo ${name#DejaVu} | sed -r 's/-/ /g; s/[SBOCI]/ \0/g')
    unset family
    weight=normal
    stretch=normal
    style=normal
    weight=normal
    for tag in $tags; do
        case $tag in
            Sans) family=dejavu-sans ;;
            Serif) family=dejavu-serif ;;
            Bold) weight=bold ;;
            Oblique) style=oblique ;;
            Condensed) stretch=condensed ;;
            ExtraLight) weight=100 ;;
            SansMono) family=dejavu-mono ;;
            Italic) style=italic ;;
            *) echo "Unkown font tag $tag" >&2; exit 1 ;;
        esac
    done
    cat <<EOF
@font-face {
  font-family: $family;
  src: url('$name.eot');
  src: url('$name.eot?#iefix') format('embedded-opentype'),
       url('$name.woff') format('woff'),
       url('$name.ttf') format('truetype');
  font-weight: $weight;
  font-style: $style;
}
EOF
done > "$here/fonts.css"
