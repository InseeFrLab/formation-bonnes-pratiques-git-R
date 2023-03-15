#!/bin/bash

cd slides
quarto render formation_light.qmd
quarto render formation_full.qmd
quarto render index.qmd
python3 -m http.server 5000
