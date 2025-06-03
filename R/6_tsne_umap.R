# ---------- tSNE_UMAP.R ----------
library(Rtsne)     # t-SNE
library(umap)      # UMAP
library(ggplot2)

set.seed(2025)
## Usamos los mismos 4 PC que en K-means
pc_mat <- as.matrix(pca_result$x[, 1:4])

# --- t-SNE ---
tsne_out <- Rtsne(pc_mat, perplexity = 30, eta = 200, dims = 2)
tsne_df  <- as.data.frame(tsne_out$Y)
colnames(tsne_df) <- c("tSNE1", "tSNE2")
tsne_df$cluster   <- factor(km_result$cluster)

ggplot(tsne_df, aes(tSNE1, tSNE2, colour = cluster)) +
  geom_point(size = 1.7, alpha = .8) +
  theme_minimal() +
  labs(title = "t-SNE de los jugadores", colour = "Cluster")
ggsave("figures/tsne_clusters.pdf", width = 6, height = 4)

# --- UMAP ---
umap_out <- umap(pc_mat, n_neighbors = 25, min_dist = .25)
umap_df  <- as.data.frame(umap_out$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$cluster   <- factor(km_result$cluster)

ggplot(umap_df, aes(UMAP1, UMAP2, colour = cluster)) +
  geom_point(size = 1.7, alpha = .8) +
  theme_minimal() +
  labs(title = "UMAP de los jugadores", colour = "Cluster")
ggsave("figures/umap_clusters.pdf", width = 6, height = 4)
