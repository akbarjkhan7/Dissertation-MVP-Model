#8) Simple statistical tests (use FINAL_MVP_SCORE)
cat("\n=== Kruskal–Wallis: FINAL_MVP_SCORE by ROLE ===\n")
kw_role <- kruskal.test(FINAL_MVP_SCORE ~ ROLE, data = player_data)
print(kw_role)

cat("\n=== Pairwise Wilcoxon (ROLE, Holm) on FINAL_MVP_SCORE ===\n")
print(pairwise.wilcox.test(player_data$FINAL_MVP_SCORE, player_data$ROLE,
                           p.adjust.method = "holm", exact = FALSE))

cat("\n=== Chi-squared: ROLE x CLUSTER (effect size Cramer's V) ===\n")
tab_rc <- with(subset(player_data, !is.na(CLUSTER)), table(ROLE, CLUSTER))
chi <- chisq.test(tab_rc, simulate.p.value = TRUE, B = 10000)  
print(chi)
n <- sum(tab_rc); r <- nrow(tab_rc); k <- ncol(tab_rc)
V <- sqrt(as.numeric(chi$statistic) / (n * (min(r - 1, k - 1))))
cat(sprintf("Cramer's V = %.3f\n", V))

cat("\n=== MVP PIPELINE COMPLETE ===\n")