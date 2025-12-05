### Sensitivity test: PCA recalculated using only complete environmental cases
# This analysis evaluates whether the small set of missing values across four
# environmental predictors affects the structure of the principal components.

# Inspect the environmental matrix used for PCA
str(envir.pca_data)
colnames(envir.pca_data)
dim(envir.pca_data)  # 8141 rows, 15 predictors

# Rows containing any missing values are identified and removed, producing a
# complete-case dataset for comparison with the main PCA.
envir_complete <- envir.pca_data[complete.cases(envir.pca_data), ]

nrow(envir_complete)     # number of rows retained
nrow(envir.pca_data)     # original row count

# PCA is recalculated on the complete-case dataset using the same centering 
# and scaling parameters as the main analysis.
pca_complete <- prcomp(envir_complete, center = TRUE, scale. = TRUE)
summary(pca_complete)

# Loadings from the complete-case PCA are extracted for the first eight 
# components, matching those used in downstream ecozone analyses.
load_full  <- envir.pca_result$rotation[, 1:8]      # original PCA loadings
load_clean <- pca_complete$rotation[, 1:8]          # complete-case loadings

# Absolute differences between corresponding loadings provide a direct measure 
# of the impact of missing-value handling on PCA structure.
loading_diff <- abs(load_full - load_clean)

# Maximum loading shifts per component summarize the stability of each axis.
apply(loading_diff, 2, max)
