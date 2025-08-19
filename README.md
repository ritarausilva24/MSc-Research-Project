# Urban Bird Communities in the City of London: Effects of Streetlighting and Tree Cover

This repository contains scripts for BirdNet data validation, data visualization and statistical analyses that will provide the results for my research project.

# Dataset
- raw data = "/Project/Data"
- final validated data set = "/Project/final.xlsx""
- batdetect_path_pattern = "/Project/metadata.xlsx"
Dataset available on request.


# Objectives
- Validade selection table outputs from BirdNET Analyzer to ensure high precision
- Create tables of descriptive statistics
- Perform models (GLMM and PERMANOVA) and a Principal Coordinate Analysis (PCoA) for assessing effects of streetlight on bird activity, species richness and community composition
- Data visualization

# Languages used
R Statistical Software (v4.4.2)

# Dependencies
R Statistical Software (v4.4.2) environment with the following libraries:
- writexl
- readxl
- dplyr
- purrr
- tidyverse
- ggplot2
- RColorBrewer
- tidyr
- glmmTMB
- DHARMa
- vegan
- patchwork
  
**Code**
## Scripts Overview

### Data validation

Overview
This script processes raw bird call data from multiple Excel files, validates species identifications, and produces a final dataset of high-confidence recordings for analysis.

Key Steps:

- Load and combine data: Reads all Excel files in the project folder and merges them into a single dataframe.
- Clean and format: Renames columns and ensures only one individual of each species per recording is counted.
- Random stratified sampling: Selects validation samples for each species based on confidence scores (high, mid, low).
- Precision calculation: Computes species-level precision (true positives vs. false positives) after validation.
- Filter high-precision species: Retains recordings from species with ≥90% identification accuracy.
- Finalize dataset: Combines validated species data and adds street IDs and types for further analysis.
- Output: Saves validation and final datasets as Excel files for downstream analyses.


### Statistical summary and plots

Overview
This project analyzes urban bird activity, species richness, and community composition across different street types (wooded vs. non-wooded). The analysis combines field observations, species frequency data, and site metadata to explore how bird communities vary over time and by habitat type.

Key steps:
- Summarizes bird observations per week and street type
- Calculates species richness and presence-absence metrics
- Compares wooded and non-wooded streets using statistical tests (t-tests)
- Generates visualizations of: Weekly bird observations, Species-specific observation trends, Mean observation changes, Species richness, Community composition over time


### Statistical models

Overview
This R script analyzes bird community data in urban streets to test two main hypotheses:(1) How streetlight intensity affects bird activity, species richness, and community composition and (H2) How street type (e.g., residential vs. main street) and its interaction with time (Week) affect the same metrics.

Key steps:
- Load libraries & data – Import bird counts, metadata, and set up packages.
- Data preparation – Aggregate counts, calculate species richness, and create presence/absence and abundance matrices.

Hypothesis 1 (Streetlight intensity) –
- GLMMs for bird activity & richness
- PERMANOVA for community composition

Hypothesis 2 (Street type × Week) –
- GLMMs with interaction for activity & richness
- PERMANOVA and PCoA for community composition

- Visualization – Plot PCoA results and summary figures.

**Author: Rita Rau Silva**
