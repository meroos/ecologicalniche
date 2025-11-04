
suppressPackageStartupMessages({ library(readr); library(dplyr); library(here); library(cluster) })

scores <- read_csv(here("output","pca_scores_1to8.csv"), show_col_types = FALSE)

silhouette_scores <- sapply(2:15, function(k) {
  km <- kmeans(scores[,1:8], centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(scores[,1:8]))
  mean(ss[,3])
})

out <- tibble::tibble(k = 2:15, silhouette = as.numeric(silhouette_scores))
readr::write_csv(out, here("output","silhouette_scores_k2_15.csv"))
message("Wrote: output/silhouette_scores_k2_15.csv")

