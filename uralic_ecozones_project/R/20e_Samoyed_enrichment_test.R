samoyedic_rf

library(dplyr)
library(stringr)
library(tidyr)
library(sf)

# 1. Drop geometry, keep only subgroup column
sam_df <- samoyedic_rf %>% 
  st_drop_geometry() %>% 
  select(Subgroup)

# 2. Add row id and dummy
sam_df$id <- seq_len(nrow(sam_df))
sam_df$dummy <- 1

# 3. Pivot wider safely
sam_df_wide <- tidyr::pivot_wider(
  sam_df,
  id_cols = id,
  names_from = Subgroup,
  values_from = dummy,
  values_fill = 0
)

# 4. Add back to full sf object
samoyedic_rf_expanded <- samoyedic_rf %>%
  mutate(id = seq_len(nrow(.))) %>%
  left_join(sam_df_wide, by = "id")


subgroup_cols <- c(
  "Kamas and Mator",
  "Tomsk region Selkup",
  "Northern Selkup",
  "Forest Nenets",
  "Tundra Nenets",
  "Forest Enets",
  "Tundra Enets",
  "Nganasan"
)

minimal_sam <- samoyedic_rf_expanded %>%
  select(all_of(subgroup_cols), Ecozone_renamed, geometry)

sam_vect <- vect(minimal_sam)
sam_vect$cell_id <- seq_len(nrow(sam_vect))
minimal_sam$cell_id <- sam_vect$cell_id

# compute cell resolution
one <- sam_vect[1]
cell_w  <- xmax(one) - xmin(one)
cell_h  <- ymax(one) - ymin(one)

# raster template
sam_r_template <- rast(
  ext = ext(sam_vect),
  resolution = c(cell_w, cell_h),
  crs = crs(sam_vect)
)

# raster of cell IDs
sam_cell_id_rast <- rasterize(sam_vect, sam_r_template, field = "cell_id")

torus_shift <- function(r, dx, dy) {
  nr <- nrow(r)
  nc <- ncol(r)
  
  dx <- dx %% nc
  dy <- dy %% nr
  
  vals <- values(r)
  m <- matrix(vals, nrow = nr, ncol = nc, byrow = FALSE)
  
  if (dx > 0) m <- m[, c((nc-dx+1):nc, 1:(nc-dx)), drop = FALSE]
  if (dy > 0) m <- m[c((nr-dy+1):nr, 1:(nr-dy)), , drop = FALSE]
  
  r2 <- r
  values(r2) <- as.vector(m)
  r2
}

# Force the ecozone factor to have levels 1-9 even if absent
minimal_sam$Ecozone_renamed <- factor(
  minimal_sam$Ecozone_renamed,
  levels = as.character(1:9)
)


run_samoyedic_torus_enrichment_studyarea <- function(sub_col, nperm = 999) {
  
  # 1) OBSERVED: compute from the real data only (no raster IDs)
  obs_eco <- minimal_sam$Ecozone_renamed[minimal_sam[[sub_col]] == 1]
  obs_eco <- factor(obs_eco, levels = levels(minimal_rf$Ecozone_renamed))
  
  obs_counts <- tabulate(
    as.integer(obs_eco),
    nbins = length(levels(minimal_rf$Ecozone_renamed))
  )
  
  # 2) NULL MODEL grid: use the SAME grid as branches
  mask_r <- rasterize(sam_vect, r_template, field = sub_col)
  mask_r <- ifel(is.na(mask_r), 0, mask_r)
  
  id_vals <- values(cell_id_rast)
  
  Nr <- nrow(mask_r)
  Nc <- ncol(mask_r)
  
  perm_counts <- matrix(0, nperm, length(obs_counts))
  
  for (i in 1:nperm) {
    dx <- sample(0:(Nc - 1), 1)
    dy <- sample(0:(Nr - 1), 1)
    
    shifted <- torus_shift(mask_r, dx, dy)
    s_vals <- values(shifted)
    
    sid <- id_vals[s_vals == 1]
    sid <- sid[!is.na(sid)]
    
    se <- minimal_rf$Ecozone_renamed[sid]
    se <- factor(se, levels = levels(minimal_rf$Ecozone_renamed))
    
    perm_counts[i, ] <- tabulate(
      as.integer(se),
      nbins = length(levels(minimal_rf$Ecozone_renamed))
    )
  }
  
  expected <- colMeans(perm_counts)
  lower    <- apply(perm_counts, 2, quantile, 0.025)
  upper    <- apply(perm_counts, 2, quantile, 0.975)
  
  obs_mat <- matrix(obs_counts, nrow = nperm, ncol = length(obs_counts), byrow = TRUE)
  pvals <- colSums(perm_counts >= obs_mat) / nperm
  
  data.frame(
    Subgroup = sub_col,
    Ecozone  = levels(minimal_rf$Ecozone_renamed),
    Observed = obs_counts,
    Expected = expected,
    Ratio    = obs_counts / expected,
    Lower    = lower,
    Upper    = upper,
    p        = pvals
  )
}

samoyedic_enrichment <- bind_rows(
  lapply(subgroup_cols, run_samoyedic_torus_enrichment_studyarea, nperm = 999)
)

# sanity check
check <- samoyedic_enrichment %>%
  group_by(Subgroup) %>%
  summarise(sum_obs = sum(Observed), .groups = "drop") %>%
  left_join(as.data.frame(table(samoyedic_rf$Subgroup)) %>%
              rename(Subgroup = Var1, total = Freq),
            by = "Subgroup") %>%
  mutate(OK = sum_obs == total)

check # all must be TRUE

head(samoyedic_enrichment)

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

samoyedic_enrichment_named <- samoyedic_enrichment %>%
  dplyr::mutate(Ecozone = as.factor(Ecozone)) %>%
  left_join(ecozone_names, by = "Ecozone") %>%
  dplyr::select(
    Subgroup, Ecozone, Ecozone_Name,
    Observed, Expected, Ratio, Lower, Upper, p
  )


head(samoyedic_enrichment_named)

write.csv(
  samoyedic_enrichment_named,
  file = "xxx",
  row.names = FALSE
)

table(samoyedic_rf$Subgroup)

samoyedic_enrichment_named %>% 
  filter(p < 0.05) %>% 
  arrange(p)

