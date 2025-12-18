library(sf)
library(spdep)
pc_df <- as.data.frame(envir.pca_result$x)

# PCA columns numeric
str(pc_df[, 1:8])

# Attaching geometry from uralic_rf
pc_sf <- st_sf(pc_df, geometry = st_geometry(uralic_rf))
str(pc_sf)
str(pc_sf[, paste0("PC", 1:8)])
pc_scaled <- scale(st_drop_geometry(pc_sf)[, paste0("PC", 1:8)])
str(pc_scaled)

# Coordinates (centroids of sf grid)
# BEFORE NA removal
pc_sf_full <- st_sf(
  as.data.frame(envir.pca_result$x),
  geometry = st_geometry(uralic_rf)
)
str(pc_sf_full)
coords <- st_coordinates(st_centroid(pc_sf))
plot(st_geometry(pc_sf_full), col = NA, border = "black")


install.packages("ClustGeo")
library(ClustGeo)

# 1. Data matrix
X <- pc_scaled   # 8141 x 8

# 2. Spatial distance matrix (centroids)
coords <- st_coordinates(st_centroid(pc_sf))
Dgeo  <- dist(coords)

# 3. Feature distance
Dvar  <- dist(X)

# 4. Tune alpha (mix weight)
# Use alpha = 0.2 or 0.3 for your ecozones
alpha <- 0.2

# 5. ClustGeo
clust_res <- hclustgeo(Dvar, Dgeo, alpha = alpha)

# 6. Cut into 9 clusters
clusters_clustgeo <- cutree(clust_res, k = 9)

pc_sf_full$cluster_clustgeo <- clusters_clustgeo

pc_scores <- envir.pca_result$x[, 1:8]   # PC1-PC8
set.seed(123)
kmeans_res <- kmeans(pc_scores, centers = 9, nstart = 50)
pc_sf_full$cluster_kmeans <- clusters_kmeans

clusters_kmeans <- kmeans_res$cluster
library(mclust)
citation("mclust")
ari <- adjustedRandIndex(
  pc_sf_full$cluster_kmeans,
  pc_sf_full$cluster_clustgeo
)


ari #0.2900054

library(ggplot2)

p1 <- ggplot(pc_sf_full) +
  geom_sf(aes(fill = factor(cluster_kmeans)), color = NA) +
  ggtitle("PCA-kmeans ecozones (no spatial constraint)") +
  theme_minimal()

p2 <- ggplot(pc_sf_full) +
  geom_sf(aes(fill = factor(cluster_clustgeo)), color = NA) +
  ggtitle("ClustGeo ecozones (spatially constrained)") +
  theme_minimal()

p1
p2

citation("ClustGeo")

# outdir <- "outputs_ecozone_comparison"
# if (!dir.exists(outdir)) dir.create(outdir)


ggsave(
  filename = file.path(outdir, "Ecozones_Kmeans.png"),
  plot = p1,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(outdir, "Ecozones_Kmeans.pdf"),
  plot = p1,
  width = 8,
  height = 6
)

# --- Save ClustGeo ecozones ---
ggsave(
  filename = file.path(outdir, "Ecozones_ClustGeo.png"),
  plot = p2,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(outdir, "Ecozones_ClustGeo.pdf"),
  plot = p2,
  width = 8,
  height = 6
)


library(dplyr)

pcs <- paste0("PC", 1:8)

# means of each PC per cluster for each method
cent_km <- pc_sf_full %>%
  st_drop_geometry() %>%
  mutate(cluster = as.factor(cluster_kmeans)) %>%
  group_by(cluster) %>%
  summarise(across(all_of(pcs), \(x) mean(x, na.rm = TRUE)), .groups = "drop")


cent_cg <- pc_sf_full %>%
  st_drop_geometry() %>%
  mutate(cluster = as.factor(cluster_clustgeo)) %>%
  group_by(cluster) %>%
  summarise(across(all_of(pcs), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# distance between kmeans-centroids and clustgeo-centroids in PCA space
D <- as.matrix(dist(rbind(cent_km[pcs], cent_cg[pcs])))
nk <- nrow(cent_km)
Dkc <- D[1:nk, (nk+1):(nk+nrow(cent_cg))]

# greedy matching: for each kmeans cluster, pick closest clustgeo centroid
match_idx <- apply(Dkc, 1, which.min)

cluster_matching <- data.frame(
  kmeans_cluster = cent_km$cluster,
  clustgeo_cluster = cent_cg$cluster[match_idx],
  centroid_distance = apply(Dkc, 1, min)
)

cluster_matching # the main results
summary(cluster_matching$centroid_distance) #statistics

library(ggplot2)

pc_plot_df <- pc_sf_full %>%
  st_drop_geometry() %>%
  select(PC1, PC2, cluster_kmeans, cluster_clustgeo)

ggplot(pc_plot_df, aes(PC1, PC2, color = factor(cluster_kmeans))) +
  geom_point(size = 0.5, alpha = 0.5) +
  labs(title = "PCA space: k-means clusters") +
  theme_minimal()

ggplot(pc_plot_df, aes(PC1, PC2, color = factor(cluster_clustgeo))) +
  geom_point(size = 0.5, alpha = 0.5) +
  labs(title = "PCA space: ClustGeo clusters") +
  theme_minimal()


