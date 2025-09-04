#6) MVP scoring (simple weights)
get_weights <- function(role){
  #metrics order: NORM_RUNS, NORM_STRIKE, NORM_AVG, NORM_WICKETS, NORM_ECONOMY, NORM_FIELDING
  if (role == "Batter")               return(c(0.40, 0.30, 0.30, 0.00, 0.00, 0.00))
  if (role == "Bowler")               return(c(0.00, 0.00, 0.00, 0.50, 0.30, 0.20))
  if (role == "Batting All-Rounder")  return(c(0.30, 0.25, 0.20, 0.15, 0.05, 0.05))
  if (role == "Bowling All-Rounder")  return(c(0.15, 0.10, 0.10, 0.35, 0.20, 0.10))
  if (role == "Wicketkeeper")         return(c(0.20, 0.20, 0.20, 0.00, 0.00, 0.40))
  #Backward compatibility if any "All-Rounder" slipped through
  if (role == "All-Rounder")          return(c(0.25, 0.20, 0.15, 0.20, 0.10, 0.10))
  rep(1/6, 6)
}

metric_mat <- player_data |> select(NORM_RUNS, NORM_STRIKE, NORM_AVG, NORM_WICKETS, NORM_ECONOMY, NORM_FIELDING)
player_data$MVP_SCORE <- mapply(function(role,i){ sum(get_weights(role) * as.numeric(metric_mat[i,]), na.rm=TRUE) }, player_data$ROLE, seq_len(nrow(player_data)))

#Final score used for modeling/stat tests
player_data <- player_data |>
  mutate(FINAL_MVP_SCORE = MVP_SCORE)

#For accuracy 

#flags for config
SAVE_OUTPUTS     <- TRUE                 #toggle saves on/off
OUTPUT_DIR       <- "tables"             #folder to write files
REPEATED_SPLITS  <- 20                   #RF stability loops
WEIGHT_JITTER_ON <- TRUE                 #sensitivity study
WEIGHT_JITTER_SD <- 0.10                 #~±10% jitter

set.seed(2024)  #global seed for reproducibility

#Required columns present?
required_cols <- c(
  "PLAYER","CLUB","ROLE",
  "RUNS_PER_INNS","BAT_SR","BAT_AVG",
  "WICKETS_PER_OVER","BOWL_ECON","DISMISSALS",
  "NORM_RUNS","NORM_STRIKE","NORM_AVG","NORM_WICKETS","NORM_ECONOMY","NORM_FIELDING",
  "MVP_SCORE","FINAL_MVP_SCORE"
)
miss_cols <- setdiff(required_cols, names(player_data))
if (length(miss_cols)) {
  stop("Required columns missing: ", paste(miss_cols, collapse = ", "))
} else {
  message("Schema check: all required columns present.")
}

#NA auditing
na_summary <- sapply(player_data[, c("RUNS_PER_INNS","BAT_SR","BAT_AVG",
                                     "WICKETS_PER_OVER","BOWL_ECON","DISMISSALS",
                                     "NORM_RUNS","NORM_STRIKE","NORM_AVG","NORM_WICKETS","NORM_ECONOMY","NORM_FIELDING",
                                     "MVP_SCORE","FINAL_MVP_SCORE")], function(x) sum(!is.finite(x) | is.na(x)))
message("NA/Inf counts (key fields):")
print(na_summary)

#Range checks
rng_warn <- function(expr, msg) { if (any(expr, na.rm = TRUE)) warning(msg, call. = FALSE) }
rng_warn(player_data$BAT_SR < 0 | player_data$BAT_SR > 350,     "Check BAT_SR outside [0,350].")
rng_warn(player_data$BAT_AVG < 0 | player_data$BAT_AVG > 150,   "Check BAT_AVG outside [0,150].")
rng_warn(player_data$BOWL_ECON < 0 | player_data$BOWL_ECON > 20,"Check BOWL_ECON outside [0,20].")
rng_warn(player_data$WICKETS_PER_OVER < 0 | player_data$WICKETS_PER_OVER > 1.5,
         "Check WICKETS_PER_OVER outside [0,1.5].")

#Normalised in [0,1]?
norm_cols <- c("NORM_RUNS","NORM_STRIKE","NORM_AVG","NORM_WICKETS","NORM_ECONOMY","NORM_FIELDING")
bad_norm <- sapply(player_data[, norm_cols], function(x) any(x < -1e-6 | x > 1 + 1e-6, na.rm = TRUE))
if (any(bad_norm)) {
  warning("Some normalised metrics fall outside [0,1]. Columns: ",
          paste(names(bad_norm)[bad_norm], collapse = ", "), call. = FALSE)
} else {
  message("Normalisation check: all metrics within [0,1].")
}

#Join sanity: duplicates on (PLAYER, CLUB)
dups <- player_data %>%
  dplyr::count(PLAYER, CLUB, name = "n") %>%
  dplyr::filter(n > 1)
if (nrow(dups)) {
  warning("Duplicate (PLAYER, CLUB) keys detected; rows:\n", paste(utils::capture.output(print(dups)), collapse = "\n"))
} else {
  message("Join check: no duplicate (PLAYER, CLUB) combinations.")
}

#Model stability: repeated splits
model_df <- player_data %>%
  dplyr::select(FINAL_MVP_SCORE, NORM_RUNS, NORM_STRIKE, NORM_AVG, NORM_WICKETS, NORM_ECONOMY, NORM_FIELDING) %>%
  stats::na.omit()

if (nrow(model_df) >= 20) {
  set.seed(2025)
  met <- replicate(REPEATED_SPLITS, {
    n <- nrow(model_df)
    idx <- sample(n, floor(0.8*n))
    tr  <- model_df[idx, ]
    te  <- model_df[-idx, , drop = FALSE]
    rf  <- randomForest(FINAL_MVP_SCORE ~ ., data = tr, ntree = 500)
    pr  <- predict(rf, te)
    rmse <- sqrt(mean((te$FINAL_MVP_SCORE - pr)^2))
    r2   <- 1 - sum((te$FINAL_MVP_SCORE - pr)^2) / sum((te$FINAL_MVP_SCORE - mean(te$FINAL_MVP_SCORE))^2)
    c(RMSE = rmse, R2 = r2)
  })
  met <- t(met); met <- as.data.frame(met)
  message(sprintf("RF stability over %d splits — RMSE median=%.4f [IQR %.4f–%.4f], R2 median=%.3f [IQR %.3f–%.3f]",
                  REPEATED_SPLITS,
                  stats::median(met$RMSE), stats::quantile(met$RMSE, .25), stats::quantile(met$RMSE, .75),
                  stats::median(met$R2),  stats::quantile(met$R2, .25),  stats::quantile(met$R2, .75)))
} else {
  message("RF stability: skipped (need at least 20 complete rows).")
}

#MVP weight sensitivity (rank robustness) 
if (WEIGHT_JITTER_ON) {
  base_w <- function(role) {
    if (role == "Batter")               return(c(0.40, 0.30, 0.30, 0.00, 0.00, 0.00))
    if (role == "Bowler")               return(c(0.00, 0.00, 0.00, 0.50, 0.30, 0.20))
    if (role == "Batting All-Rounder")  return(c(0.30, 0.25, 0.20, 0.15, 0.05, 0.05))
    if (role == "Bowling All-Rounder")  return(c(0.15, 0.10, 0.10, 0.35, 0.20, 0.10))
    if (role == "Wicketkeeper")         return(c(0.20, 0.20, 0.20, 0.00, 0.00, 0.40))
    #fallback
    rep(1/6, 6)
  }
  
  #baseline ranks
  base_rank <- rank(-player_data$MVP_SCORE, ties.method = "average")
  #jitter 50 times
  set.seed(2026)
  rho <- replicate(50, {
    W <- t(sapply(player_data$ROLE, function(r) {
      w <- base_w(r)
      jitter <- stats::rnorm(length(w), 0, WEIGHT_JITTER_SD)
      w2 <- pmax(0, w + jitter); if (sum(w2) == 0) w2[1] <- 1
      w2 / sum(w2)
    }))
    #compute new scores row-wise with jittered role weights
    x <- as.matrix(player_data[, c("NORM_RUNS","NORM_STRIKE","NORM_AVG","NORM_WICKETS","NORM_ECONOMY","NORM_FIELDING")])
    x[!is.finite(x)] <- 0
    new_scores <- rowSums(W * x)
    cor(base_rank, rank(-new_scores), method = "spearman", use = "complete.obs")
  })
  message(sprintf("MVP weight sensitivity — Spearman ρ median=%.3f [IQR %.3f–%.3f]",
                  stats::median(rho, na.rm = TRUE),
                  stats::quantile(rho, .25, na.rm = TRUE),
                  stats::quantile(rho, .75, na.rm = TRUE)))
}

#saves
if (SAVE_OUTPUTS) {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(player_data, file.path(OUTPUT_DIR, "mvp_cleaned_scored.csv"), row.names = FALSE)
  #useful small summaries
  role_summary <- player_data %>% dplyr::group_by(ROLE) %>%
    dplyr::summarise(n=dplyr::n(), mean_MVP=mean(MVP_SCORE, na.rm=TRUE), .groups="drop")
  utils::write.csv(role_summary, file.path(OUTPUT_DIR, "summary_role.csv"), row.names = FALSE)
  #record environment for reproducibility
  sink(file.path(OUTPUT_DIR, "sessionInfo.txt")); print(sessionInfo()); sink()
  message("Files saved in '", normalizePath(OUTPUT_DIR, winslash = "/"), "': ",
          "mvp_cleaned_scored.csv, summary_role.csv, sessionInfo.txt")
}


#Leaderboards
library(dplyr)

#Top 10 overall
top10 <- player_data %>%
  dplyr::filter(!is.na(MVP_SCORE)) %>%
  dplyr::arrange(dplyr::desc(MVP_SCORE)) %>%
  dplyr::slice_head(n = 10)

print(
  ggplot(top10, aes(x = reorder(PLAYER, MVP_SCORE), y = MVP_SCORE, fill = ROLE)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top 10 MVPs (Overall)", x = "Player", y = "MVP Score") +
    theme_minimal()
)

#Top 5 by role
top_by_role <- player_data %>%
  dplyr::filter(!is.na(ROLE), !is.na(MVP_SCORE)) %>%
  dplyr::group_by(ROLE) %>%
  dplyr::slice_max(order_by = MVP_SCORE, n = 5, with_ties = FALSE) %>%
  dplyr::ungroup()

print(
  ggplot(top_by_role, aes(x = reorder(PLAYER, MVP_SCORE), y = MVP_SCORE, fill = ROLE)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ ROLE, scales = "free_y") +
    labs(title = "Top 5 MVPs by Role", x = "Player", y = "MVP Score") +
    theme_minimal()
)

#MVP by cluster
print(
  player_data %>%
    dplyr::filter(!is.na(MVP_SCORE), !is.na(CLUSTER)) %>%
    ggplot(aes(CLUSTER, MVP_SCORE, fill = CLUSTER)) +
    geom_boxplot(outlier.alpha = 0.25) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = "MVP Score by Cluster", x = "Cluster", y = "MVP Score")
)

#MVP summaries
cat("\n=== MVP Summary by Role ===\n")
print(fmt(player_data |> group_by(ROLE) |> summarise(n=n(), mean_MVP=mean(MVP_SCORE, na.rm=TRUE), sd_MVP=sd(MVP_SCORE, na.rm=TRUE), median_MVP=median(MVP_SCORE, na.rm=TRUE))))
cat("\n=== MVP Summary by Cluster ===\n")
print(fmt(player_data |> filter(!is.na(CLUSTER)) |> group_by(CLUSTER) |> summarise(n=n(), mean_MVP=mean(MVP_SCORE, na.rm=TRUE), sd_MVP=sd(MVP_SCORE, na.rm=TRUE), median_MVP=median(MVP_SCORE, na.rm=TRUE))))
