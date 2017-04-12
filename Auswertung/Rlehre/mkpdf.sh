#!/bin/bash

for f in $(ls -1 *.svg)
do
  svg2pdf "$f" "$(basename ${f} .svg).pdf"
done
