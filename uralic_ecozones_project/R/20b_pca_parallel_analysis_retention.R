library(ggplot2)
library(vegan)

pca <- prcomp(envir.pca_data, center = TRUE, scale. = TRUE)
eigvals <- pca$sdev^2
bs <- bstick(length(eigvals))

df <- data.frame(
  PC = 1:length(eigvals),
  Eigenvalue = eigvals,
  BrokenStick = bs
)

ggplot(df, aes(PC, Eigenvalue)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = BrokenStick), linetype = "dashed") +
  ylab("Eigenvalue") +
  xlab("Principal Component")


library(psych)

pa <- fa.parallel(envir.pca_data, fa = "pc", n.iter = 1000, show.legend = FALSE)

# Extracting observed vs simulated eigenvalues
obs  <- pa$pc.values
sim  <- pa$pc.sim

# PCs that pass the threshold
retain <- which(obs > sim) # 1 2 3 4
retain <- 1:4
# PCs above the parallel-analysis threshold (likely 1-5)
pcs_keep <- pca$x[, 1:5]

set.seed(123)
km_PA <- kmeans(pcs_keep, centers = 9, nstart = 50)

# Comparing to full PC1-PC8 clustering
pcs_full <- pca$x[, 1:8]
km_full <- kmeans(pcs_full, centers = 9, nstart = 50)

table(km_PA$cluster, km_full$cluster)

df <- data.frame(
  PC = 1:length(obs),
  Observed = obs,
  Simulated = sim
)

library(ggplot2)

p <- ggplot(df, aes(x = PC)) +
  geom_line(aes(y = Observed)) +
  geom_point(aes(y = Observed)) +
  geom_line(aes(y = Simulated), linetype = "dashed") +
  ylab("Eigenvalue") +
  xlab("Principal Component") +
  theme_minimal()

p

ggsave("PA_scree_plot.png", p, width = 7, height = 5, dpi = 300)

