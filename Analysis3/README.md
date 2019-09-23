Analysis 3
====

## Overview
This folder includes rstan codes, graphing codes, and data for the section "Analysis 3" in the following paper:

Yamamoto, H., Sato, A., & Itakura, S. (2019). Eye tracking in an everyday environment reveals the interpersonal distance that affords infant-parent gaze communication. Scientific reports, 9(1), 10352.
https://doi.org/10.1038/s41598-019-46650-6

## Description
It consists maily of two folders and four files:
- StanModels
  - This folder includes rstan codes for Analysis 3.
- StanResults
  - The folder for files including MCMC samples.
- Data_processed.csv
  - Data already processed in the "preprocess" folder.
- compareModels.R
  - code for model comparison.
- compareModels_result.R
  - code for model comparison (table S3).
- createGraphs.R
  - graphing codes (figure S5)

## Usage
Run codes in the following order.
1. compareModels.R
3. compareModels_result.R
3. createGraphs.R

## Author
[Hiroki Yamamoto](https://github.com/dororo1225)
