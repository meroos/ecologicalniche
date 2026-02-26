This repository contains the environmental data, language presence–absence grid data, analysis scripts, and source data necessary to reproduce all results and figures reported in the manuscript.
SOFTWARE
R version X.X.X
Key packages: ranger, sf, terra, caret, pROC, ggplot2
Full session information available in scripts/00_session_info.R

REPRODUCTION INSTRUCTIONS

Run scripts/01_prepare_data.R

Run scripts/02_PCA_clustering.R

Run scripts/03_enrichment_test.R

Run scripts/04_random_forest_models.R

Run scripts/05_generate_figures.R

DATA DESCRIPTION

data_raw/
Description of contents

data_processed/
Description

source_data_figures/
Contains CSV files corresponding to all figures and tables
