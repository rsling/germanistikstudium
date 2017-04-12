#!/bin/bash

set -e
set -u

cp /Users/user/Data/linguistics/ulrike/Grammatiklehre/Auswertung/Rlehre/*.svg .
cp /Users/user/Data/linguistics/ulrike/Grammatiklehre/Auswertung/Rlehre/results.txt .

for f in $(ls -1 *.svg)
do
  svg2pdf "$f" "$(basename ${f} .svg).pdf"
done
