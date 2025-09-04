#4) PCA (structure) + tables
pca_vars   <- player_data |> select(NORM_RUNS, NORM_STRIKE, NORM_AVG, NORM_WICKETS, NORM_ECONOMY, NORM_FIELDING)
idx_comp   <- complete.cases(pca_vars)
pca_input  <- pca_vars[idx_comp, , drop = FALSE]
pca_result <- prcomp(pca_input, scale. = TRUE)

#Scree
var_exp <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
cum_exp <- cumsum(var_exp)
plot(var_exp, type="b", pch=19, main="PCA Scree Plot", xlab="PC", ylab="Proportion of Variance")
lines(cum_exp, lty=2); points(cum_exp, pch=1); legend("topright", c("Variance","Cumulative"), lty=c(1,2), pch=c(19,1), bty="n")

#PC1-PC2 by role
scores <- as.data.frame(pca_result$x[,1:2]); colnames(scores) <- c("PC1","PC2")
scores$ROLE <- player_data$ROLE[idx_comp]
print(
  ggplot(scores, aes(PC1, PC2, colour = ROLE)) +
    geom_point(alpha=0.85) +
    labs(title="PCA Scatter: PC1 vs PC2 (by Role)") +
    theme_minimal()
)

#Biplot
print(factoextra::fviz_pca_biplot(pca_result, repel=TRUE, label="var", title="PCA Biplot (Variables)"))

#PCA tables
cat("\n=== PCA: Eigenvalues & Variance ===\n")
print(fmt(data.frame(PC=1:length(pca_result$sdev), Eigenvalue=pca_result$sdev^2, PropVar=var_exp, CumVar=cum_exp)))
cat("\n=== PCA: Loadings (PC1–PC3) ===\n")
print(fmt(as.data.frame(pca_result$rotation[,1:3])))

#5) Clustering (k=4 on PC1–PC3) + silhouette
pc_scores <- as.data.frame(pca_result$x[,1:3])
set.seed(42)
km <- kmeans(pc_scores, centers = 4, nstart = 25)

pc12 <- pc_scores[,1:2]; colnames(pc12) <- c("PC1","PC2")
pc12$CLUSTER <- factor(km$cluster)
cent <- as.data.frame(km$centers[,1:2]); colnames(cent) <- c("PC1","PC2")

print(
  ggplot(pc12, aes(PC1, PC2, colour = CLUSTER)) +
    geom_point(alpha=0.85) +
    geom_point(data=cent, aes(PC1,PC2), colour="black", size=3, shape=8) +
    labs(title="K-means on PCA Scores (PC1–PC2)") + theme_minimal()
)

cat("\n=== Cluster Sizes ===\n"); print(table(km$cluster))
sil <- silhouette(km$cluster, dist(pc_scores))
cat("\n=== Mean Silhouette Width ===\n"); print(mean(sil[, "sil_width"]))

#attach clusters back to full data (NAs for incomplete rows)
player_data$CLUSTER <- NA_integer_; player_data$CLUSTER[idx_comp] <- km$cluster; player_data$CLUSTER <- factor(player_data$CLUSTER)