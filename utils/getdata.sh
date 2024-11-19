#!/bin/bash

git clone https://github.com/InseeFrLab/formation-bonnes-pratiques-exo-correction.git
rm -rf ./formation-bonnes-pratiques-exo-correction/.git
mc cp s3/projet-formation/bonnes-pratiques/data/RPindividus.parquet ./formation-bonnes-pratiques-exo-correction/RPindividus.parquet