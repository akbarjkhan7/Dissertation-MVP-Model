#7) ML model: Random Forest (+ Linear baseline)

cat("\n=== STEP 7: MODELING (RF baseline) ===\n")

#helper
.has_kable <- "knitr" %in% .packages()

#Prepare modeling data
model_data <- player_data |>
  dplyr::select(FINAL_MVP_SCORE, NORM_RUNS, NORM_STRIKE, NORM_AVG,
                NORM_WICKETS, NORM_ECONOMY, NORM_FIELDING) |>
  na.omit()

n_all <- nrow(model_data)
cat("Model training data prepared:", n_all, "complete cases\n")
if (n_all < 3) stop("Not enough complete rows to model. Check FINAL_MVP_SCORE & predictors for NAs.")

#Train/test split (robust for small n)
set.seed(123)
if (n_all >= 10) {
  n_train   <- floor(0.8 * n_all)
  train_idx <- sample(n_all, n_train)
  train_data <- model_data[train_idx, ]
  test_data  <- model_data[-train_idx, , drop = FALSE]
} else {
  warning("Few rows available; using all rows for both training and testing (diagnostic only).")
  train_data <- model_data
  test_data  <- model_data
}

#Containers
model_results      <- list()
model_predictions  <- data.frame(Actual = test_data$FINAL_MVP_SCORE)

#Random Forest (main model)
cat("Training Random Forest...\n")
set.seed(42)
rf_model <- randomForest(FINAL_MVP_SCORE ~ ., data = train_data,
                         ntree = 500, importance = TRUE)
rf_pred <- predict(rf_model, test_data)
model_predictions$RF <- rf_pred

model_results$RF <- list(
  RMSE = sqrt(mean((test_data$FINAL_MVP_SCORE - rf_pred)^2)),
  R2   = 1 - sum((test_data$FINAL_MVP_SCORE - rf_pred)^2) /
    sum((test_data$FINAL_MVP_SCORE - mean(test_data$FINAL_MVP_SCORE))^2),
  MAE  = mean(abs(test_data$FINAL_MVP_SCORE - rf_pred))
)

#Linear Regression (baseline)
cat("Training Linear Regression...\n")
lm_model <- lm(FINAL_MVP_SCORE ~ ., data = train_data)
lm_pred  <- predict(lm_model, test_data)
model_predictions$Linear <- lm_pred

model_results$Linear <- list(
  RMSE = sqrt(mean((test_data$FINAL_MVP_SCORE - lm_pred)^2)),
  R2   = 1 - sum((test_data$FINAL_MVP_SCORE - lm_pred)^2) /
    sum((test_data$FINAL_MVP_SCORE - mean(test_data$FINAL_MVP_SCORE))^2),
  MAE  = mean(abs(test_data$FINAL_MVP_SCORE - lm_pred))
)

#Elastic Net baseline
use_elastic_net <- FALSE
if (use_elastic_net) {
  if (!requireNamespace("glmnet", quietly = TRUE)) stop("glmnet not installed.")
  library(glmnet)
  x_train <- model.matrix(FINAL_MVP_SCORE ~ ., data = train_data)[, -1]
  y_train <- train_data$FINAL_MVP_SCORE
  x_test  <- model.matrix(FINAL_MVP_SCORE ~ ., data = test_data)[, -1]
  set.seed(42)
  cv_en   <- cv.glmnet(x_train, y_train, alpha = 0.5, nfolds = 10)
  en_model <- glmnet(x_train, y_train, alpha = 0.5, lambda = cv_en$lambda.min)
  en_pred  <- as.numeric(predict(en_model, x_test))
  model_predictions$ElasticNet <- en_pred
  model_results$ElasticNet <- list(
    RMSE = sqrt(mean((test_data$FINAL_MVP_SCORE - en_pred)^2)),
    R2   = 1 - sum((test_data$FINAL_MVP_SCORE - en_pred)^2) /
      sum((test_data$FINAL_MVP_SCORE - mean(test_data$FINAL_MVP_SCORE))^2),
    MAE  = mean(abs(test_data$FINAL_MVP_SCORE - en_pred))
  )
} 

#XGBoost (boosted trees comparator)
cat("Training XGBoost...\n")

#Prepare numeric matrices with same predictors as RF/Linear
x_cols <- setdiff(names(train_data), "FINAL_MVP_SCORE")

#Ensure all predictors are numeric
num_cols <- x_cols[sapply(train_data[x_cols], is.numeric)]
if (length(num_cols) == 0) stop("XGBoost: no numeric predictors available.")

x_train <- as.matrix(train_data[ , num_cols, drop = FALSE])
y_train <- as.numeric(train_data$FINAL_MVP_SCORE)

x_test  <- as.matrix(test_data [ , num_cols, drop = FALSE])
y_test  <- as.numeric(test_data$FINAL_MVP_SCORE)

#Guard against NAs in matrices
fix_na <- function(M, ref) {
  for (j in seq_len(ncol(M))) {
    v <- M[, j]
    if (any(!is.finite(v))) {
      med <- stats::median(ref[, j], na.rm = TRUE)
      if (!is.finite(med)) med = 0
      v[!is.finite(v)] <- med
      M[, j] <- v
    }
  }
  M
}
x_train <- fix_na(x_train, x_train)
x_test  <- fix_na(x_test,  x_train)

#Train model
set.seed(42)
xgb_model <- xgboost(
  data = x_train,
  label = y_train,
  objective = "reg:squarederror",
  nrounds = 300,
  max_depth = 4,
  eta = 0.08,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  verbose = 0
)

#Predictions & metrics
xgb_pred <- predict(xgb_model, x_test)
model_predictions$XGBoost <- xgb_pred

model_results$XGBoost <- list(
  RMSE = sqrt(mean((y_test - xgb_pred)^2)),
  R2   = 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2),
  MAE  = mean(abs(y_test - xgb_pred))
)

#Importance table
xgb_imp <- xgboost::xgb.importance(model = xgb_model, feature_names = colnames(x_train))
xgb_imp_tbl <- as.data.frame(xgb_imp[, c("Feature","Gain","Cover","Frequency")], stringsAsFactors = FALSE)

cat("\n=== XGBoost Variable Importance (Gain) ===\n")
if (.has_kable) {
  print(knitr::kable(fmt(xgb_imp_tbl, 3), caption = "XGBoost Variable Importance (Gain)"))
} else {
  print(fmt(xgb_imp_tbl, 3))
}


#Comparison table
comparison_table <- do.call(rbind, lapply(names(model_results), function(model) {
  data.frame(
    Model = model,
    RMSE  = model_results[[model]]$RMSE,
    R2    = model_results[[model]]$R2,
    MAE   = model_results[[model]]$MAE
  )
})) |>
  dplyr::arrange(RMSE) |>
  dplyr::mutate(Rank = dplyr::row_number())

cat("\n=== MODEL PERFORMANCE (Ranked by RMSE) ===\n")
if (.has_kable) {
  print(knitr::kable(fmt(comparison_table, 4), caption = "Model Performance (RF vs Linear [+ ENet])"))
} else {
  print(fmt(comparison_table, 4))
}

#RF visuals and importance
varImpPlot(rf_model, main = "Random Forest Feature Importance")

print(
  ggplot(model_predictions, aes(x = Actual, y = RF)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_abline(linetype = "dashed", color = "red") +
    labs(title = "RF: Predicted vs Actual MVP (test set)",
         x = "Actual MVP", y = "Predicted MVP")
)

imp <- importance(rf_model)
feature_importance <- data.frame(
  Feature      = rownames(imp),
  RF_IncMSE    = imp[, "%IncMSE"],
  RF_IncPurity = imp[, "IncNodePurity"],
  row.names = NULL
)

cat("\n=== RF Variable Importance (table) ===\n")
if (.has_kable) {
  print(knitr::kable(fmt(feature_importance, 3), caption = "Random Forest Variable Importance"))
} else {
  print(fmt(feature_importance, 3))
}

#XGBoost Predicted vs Actual plot
print(
  ggplot(model_predictions, aes(x = Actual, y = XGBoost)) +
    geom_point(alpha = 0.6) +
    geom_abline(linetype = "dashed") +
    labs(title = "XGBoost: Predicted vs Actual MVP (test set)",
         x = "Actual MVP", y = "Predicted MVP")
)