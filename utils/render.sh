#!/bin/bash

cd slides
quarto render formation_light.qmd
python3 -m http.server 5000
