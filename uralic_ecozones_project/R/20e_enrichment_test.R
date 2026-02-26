install.packages("terra")
library(terra)      # torus shifting, rasterize, SpatRaster logic

minimal_rf <- uralic_rf %>%
  dplyr::select(
    Finnic, Hungarian, Khanty, Mansi, Mari, 
    Mordvin, Permic, Saami, Samoyedic,
    Ecozone_renamed,
    geometry
  )

rf_vect <- vect(minimal_rf)
rf_vect$cell_id <- seq_len(nrow(rf_vect))
minimal_rf$cell_id <- rf_vect$cell_id
one_cell <- rf_vect[1]
cell_width  <- xmax(one_cell) - xmin(one_cell)
cell_height <- ymax(one_cell) - ymin(one_cell)

r_template <- rast(
  ext = ext(rf_vect),
  resolution = c(cell_width, cell_height),
  crs = crs(rf_vect)
)
cell_id_rast <- rasterize(rf_vect, r_template, field = "cell_id")
max(values(cell_id_rast), na.rm = TRUE) == nrow(minimal_rf)


library(terra)
library(sf)
library(dplyr)


# ---------------------------------------------------------
# 1. TORUS SHIFT
# ---------------------------------------------------------
torus_shift <- function(r, dx, dy) {
  nr <- nrow(r)
  nc <- ncol(r)
  
  dx <- dx %% nc
  dy <- dy %% nr
  
  vals <- values(r)  # extract in correct raster order
  
  # reshape to matrix (column-major)
  m <- matrix(vals, nrow = nr, ncol = nc, byrow = FALSE)
  
  # shift horizontally
  if (dx > 0) {
    m <- m[, c((nc - dx + 1):nc, 1:(nc - dx)), drop = FALSE]
  }
  
  # shift vertically
  if (dy > 0) {
    m <- m[c((nr - dy + 1):nr, 1:(nr - dy)), , drop = FALSE]
  }
  
  # rebuild raster
  r2 <- r
  values(r2) <- as.vector(m)
  
  return(r2)
}

# ---------------------------------------------------------
# 2. TORUS ENRICHMENT TEST
# ---------------------------------------------------------
run_torus_enrichment <- function(lang_col, nperm = 199) {
  
  # 1. Rasterize language mask
  mask_rast <- rasterize(rf_vect, r_template, field = lang_col)
  mask_rast <- ifel(is.na(mask_rast), 0, mask_rast)
  
  id_vals   <- values(cell_id_rast)
  mask_vals <- values(mask_rast)
  
  # observed IDs
  obs_ids <- id_vals[mask_vals == 1]
  obs_ids <- obs_ids[!is.na(obs_ids)]
  
  obs_eco <- minimal_rf$Ecozone_renamed[obs_ids]
  
  # ---- FORCE OBSERVED COUNTS TO BE LENGTH 9 ----
  obs_counts <- tabulate(
    as.integer(factor(obs_eco, levels = levels(minimal_rf$Ecozone_renamed))),
    nbins = length(levels(minimal_rf$Ecozone_renamed))
  )
  
  # matrix for permutations
  perm_counts <- matrix(0, nperm, length(obs_counts))
  
  nc <- ncol(mask_rast)
  nr <- nrow(mask_rast)
  
  # permutations
  for (i in 1:nperm) {
    dx <- sample(0:(nc - 1), 1)
    dy <- sample(0:(nr - 1), 1)
    
    shifted <- torus_shift(mask_rast, dx, dy)
    shift_vals <- values(shifted)
    
    shift_ids <- id_vals[shift_vals == 1]
    shift_ids <- shift_ids[!is.na(shift_ids)]
    
    shift_eco <- minimal_rf$Ecozone_renamed[shift_ids]
    
    # ---- FORCE SHIFT COUNTS TO BE LENGTH 9 ----
    counts <- tabulate(
      as.integer(factor(shift_eco, levels = levels(minimal_rf$Ecozone_renamed))),
      nbins = length(levels(minimal_rf$Ecozone_renamed))
    )
    
    perm_counts[i, ] <- counts
  }
  
  expected <- colMeans(perm_counts)
  lower    <- apply(perm_counts, 2, quantile, 0.025)
  upper    <- apply(perm_counts, 2, quantile, 0.975)
  
  # ---- IMPORTANT: compare same-length vectors ----
  obs_mat <- matrix(obs_counts, nrow = nperm, ncol = length(obs_counts), byrow = TRUE)
  
  pvals <- colSums(perm_counts >= obs_mat) / nperm
  
  data.frame(
    Language = lang_col,
    Ecozone  = levels(minimal_rf$Ecozone_renamed),
    Observed = obs_counts,
    Expected = expected,
    Ratio    = obs_counts / expected,
    Lower    = lower,
    Upper    = upper,
    p        = pvals
  )
}


table(uralic_rf$Ecozone_renamed)
# ---------------------------------------------------------
# 3. ENRICHMENT FOR ALL LANGUAGES
# ---------------------------------------------------------
enrichment_test <- bind_rows(
  lapply(language_cols, function(L) run_torus_enrichment(L, nperm = 999))
)

head(enrichment_test)

ecozone_names <- data.frame(
  Ecozone = as.factor(1:9),
  Ecozone_Name = c(
    "North-Atlantic fjord landscape",
    "Cold continental mountains",
    "Subarctic tundra",
    "Boreal taiga zone",
    "Continental highlands",
    "Temperate grasslands and xeric shrublands",
    "Temperate broadleaf forests",
    "Arctic tundra",
    "Subarctic peatlands"
  )
)

enrichment_named <- enrichment_test %>%    # result
  left_join(ecozone_names, by = "Ecozone") %>%
  select(Language, Ecozone, Ecozone_Name, Observed, Expected,
         Ratio, Lower, Upper, p)


# Define output path
out_dir <- "xxx"

# Ensure directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Save the enrichment table
write.csv(enrichment_named,
          file = file.path(out_dir, "Ecozone_enrichment_results.csv"),
          row.names = FALSE)

message("Saved: ", file.path(out_dir, "Ecozone_enrichment_results.csv"))


