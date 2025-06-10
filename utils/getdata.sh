#!/bin/bash

git clone https://github.com/InseeFrLab/formation-bonnes-pratiques-exo-correction.git
rm -rf ./formation-bonnes-pratiques-exo-correction/.git
mc cp s3/projet-formation/diffusion/bonnes-pratiques/data/RPindividus_24.csv RPindividus_24.csv
