str(uralic_rf)
geom <- st_geometry(uralic_rf)

pc_sf <- st_sf(as.data.frame(envir.pca_result$x), geometry = geom)
st_crs(pc_sf) <- st_crs(uralic_rf)

pc_sf <- st_sf(
  as.data.frame(envir.pca_result$x),
  geometry = geom
)
st_crs(pc_sf) <- st_crs(uralic_rf)

# master object
str(pc_sf)


library(sf)
library(spdep)

# Moran's I for PC1 and PC2
# Data: envir_pca must contain PC1, PC2 + geometry
coords <- st_coordinates(st_centroid(pc_sf))


nb <- knearneigh(
  coords,
  k = 8,
  longlat = FALSE,
  use_kd_tree = FALSE
) |> knn2nb()

nrow(pc_sf)
length(nb)

lw <- nb2listw(nb, style = "W")
moran_PC1 <- moran.test(pc_sf$PC1, lw)
moran_PC2 <- moran.test(pc_sf$PC2, lw)

moran_PC1
moran_PC2



