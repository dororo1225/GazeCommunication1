Analysis 2
====

## Overview
This folder includes rstan codes, graphing codes, and data for the section "Analysis 2" in the following paper:

Yamamoto, H., Sato, A., & Itakura, S. (2019). Eye tracking in an everyday environment reveals the interpersonal distance that affords infant-parent gaze communication. Scientific reports, 9(1), 10352.
https://doi.org/10.1038/s41598-019-46650-6

## Description
It consists maily of two folders and seven files:
- StanModels
  - This folder includes rstan codes for Analysis 2.
- StanResults
  - The folder for files including MCMC samples.
- Data_processed.csv
  - Data already processed in the "preprocess" folder.
- compareModels_step1.R
  - code for model comparison.
- compareModels_step2.R
  - code for model comparison.
- compareModels_step3.R
  - code for model comparison.
- compareModels_step4.R
  - code for model comparison.
- compareModels_result.R
  - code for model comparison (table S3).
- createGraphs.R
  - graphing codes (figure S4)

## Usage
Run codes in the following order.
1. compareModels_step1.R
2. compareModels_step2.R
3. compareModels_step3.R
4. compareModels_step4.R
5. compareModels_result.R
6. createGraphs.R

## Author
[Hiroki Yamamoto](https://github.com/dororo1225)
