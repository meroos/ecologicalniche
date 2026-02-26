# Data and Code for Manuscript Submission  
*(Anonymised)*

## Overview

This repository contains all data and R scripts required to reproduce the analyses presented in the submitted manuscript on ecological niche modelling of language ranges across Northern Eurasia.

The workflow consists of two components:

1. **Ecozone delineation**
   - PCA of environmental variables
   - Ecozone clustering
   - Spatial permutation (torus) enrichment tests

2. **Random Forest modelling**
   - Presence–absence classification per language
   - Model evaluation (AUC, ROC, confusion matrices)
   - Variable importance and partial dependence
   - Predicted probability maps

The repository is anonymised for double-anonymous peer review.

---

## Structure

```
uralic_ecozones_project/
uralic_rf_modeling_project/
README.md
LICENSE
```

---

## Ecozones: `uralic_ecozones_project/`

```
data/     Input environmental data and derived PCA/ecozone objects
R/        Analysis scripts
output/   Generated tables and figures (CSV + PDF/JPEG)
```

Core scripts (run in order):

- `10_load_inputs.R`
- `20_pca.R`
- `30_cluster_ecozones.R`
- `20e_enrichment_test.R`
- `45_ecozone_map.R`

Outputs include:

- PCA loadings and scores (CSV)
- Ecozone assignments
- Silhouette scores
- Ecozone-language proportion tables
- Figures and maps

---

## Random Forest: `uralic_rf_modeling_project/`

```
data/     Modelling grid data
R/        Modelling scripts
output/   Model summaries, maps, figures (CSV + PNG/JPEG)
main.R    Runs full modelling workflow
```

To run full modelling workflow:

```r
source("uralic_rf_modeling_project/main.R")
```

Outputs include:

- Model evaluation summaries (CSV)
- AUC and threshold results
- Confusion matrices
- Variable importance tables
- Partial dependence data
- Predicted probability maps

All numeric results underlying figures are provided as machine-readable `.csv` files.

---

## Reproducibility

Analyses were conducted in R.

Session and package information:

```
session_info.txt
```

All scripts use relative paths and can be executed from the repository root.

---


## License

See `LICENSE` file.
