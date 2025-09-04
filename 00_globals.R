#Libraries 
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(cluster)
  library(factoextra)
  library(randomForest)
  library(e1071)
  library(xgboost)
  
})

#console formatting helper
options(width = 120)
fmt <- function(df, d = 3) {
  df <- as.data.frame(df)  
  is_num <- vapply(df, is.numeric, TRUE)
  if (any(is_num)) df[, is_num] <- lapply(df[, is_num, drop = FALSE], round, d)
  df
}

#Paths
batting_path  <- "C:/Users/Dell/Downloads/batting.xlsx"
bowling_path  <- "C:/Users/Dell/Downloads/bowling.xlsx"
fielding_path <- "C:/Users/Dell/Downloads/fielding.xlsx"

#Helpers
to_numeric <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", as.character(x))))
minmax <- function(x){
  x <- as.numeric(x); r <- range(x, na.rm = TRUE)
  if (!is.finite(r[1]) || !is.finite(r[2])) return(rep(0, length(x)))
  if (diff(r) == 0) return(rep(0.5, length(x)))
  (x - r[1]) / (r[2] - r[1])
}
clean_names <- function(df){
  names(df) <- toupper(names(df)); names(df) <- str_replace_all(names(df), " ", "_"); df
}

cat("\n=== SIMPLE MVP PIPELINE START ===\n")