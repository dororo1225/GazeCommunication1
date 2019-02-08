preprocess
====

## Overview
This folder includes rstan codes, graphing codes, and data for data processing section in "Interpersonal distance modulates infant-parent gaze communication: Evidence from eye tracking in an everyday environment" by Yamamoto, Sato and Itakura.

## Description
It consists maily of two folders and four files:
- StanModels
  - This folder includes rstan codes to define eye contact session.
- StanResults
  - The folder for files including MCMC samples.
- Data_raw.csv
  - raw data.
- Data_locomotion.csv
  - locomotor status data.
- preprocess.R
  - code for preprocessing raw data.
- createGraphs.R
  - graphing codes (figure S1, figure S2)

## Usage
Run codes in the following order.
1. preprocess.R
2. createGraphs.R

## Author
[Hiroki Yamamoto](https://github.com/dororo1225)
