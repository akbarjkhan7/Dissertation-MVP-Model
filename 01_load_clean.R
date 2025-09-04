#1) Loading & clean
batting_raw  <- read_excel(batting_path)
bowling_raw  <- read_excel(bowling_path)
fielding_raw <- read_excel(fielding_path)

batting  <- clean_names(batting_raw)
bowling  <- clean_names(bowling_raw)
fielding <- clean_names(fielding_raw)

batting <- batting |>
  rename(FIFTIES=`50S`, HUNDREDS=`100S`,
         BAT_GAMES=GAMES, BAT_INNS=INNS, BAT_NOT_OUTS=NOT_OUTS,
         BAT_RUNS=RUNS, BAT_HIGH_SCORE=HIGH_SCORE, BAT_AVG=AVG, BAT_SR=STRIKE_RATE)

bowling <- bowling |>
  rename(BOWL_OVERS=OVERS, BOWL_MAIDENS=MAIDENS, BOWL_RUNS=RUNS, BOWL_WKTS=WICKETS,
         BOWL_BBI=BEST_BOWLING, BOWL_5W=`5_WICKET_HAUL`, BOWL_ECON=ECONOMY_RATE,
         BOWL_SR=STRIKE_RATE, BOWL_AVG=AVERAGE)

fielding <- fielding |>
  rename(KEEP_CATCHES=WICKET_KEEPING_CATCHES, KEEP_STUMPINGS=STUMPINGS,
         FIELD_CATCHES=FIELDING_CATCHES, FIELD_RUNOUTS=RUN_OUTS)

batting <- batting |>
  mutate(across(any_of(c("BAT_GAMES","BAT_INNS","BAT_NOT_OUTS","BAT_RUNS","BAT_AVG","BAT_SR",
                         "FIFTIES","HUNDREDS")), to_numeric))
bowling <- bowling |>
  mutate(across(any_of(c("BOWL_OVERS","BOWL_MAIDENS","BOWL_RUNS","BOWL_WKTS","BOWL_5W",
                         "BOWL_ECON","BOWL_SR","BOWL_AVG")), to_numeric))
fielding <- fielding |>
  mutate(across(any_of(c("KEEP_CATCHES","KEEP_STUMPINGS","FIELD_CATCHES","FIELD_RUNOUTS")), to_numeric))

#light participation filters
batting <- batting |> filter((is.na(BAT_GAMES) | BAT_GAMES >= 3) | (is.na(BAT_INNS) | BAT_INNS >= 3))
bowling <- bowling |> filter(!is.na(BOWL_OVERS) & BOWL_OVERS >= 10)

#aggregate fielding per player-club
fielding <- fielding |>
  group_by(PLAYER, CLUB) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(TOTAL_CATCHES = coalesce(KEEP_CATCHES, 0) + coalesce(FIELD_CATCHES, 0),
         TOTAL_VICTIMS = TOTAL_CATCHES + coalesce(KEEP_STUMPINGS, 0))

#join
player_data <- batting |>
  full_join(bowling,  by = c("PLAYER","CLUB")) |>
  full_join(fielding, by = c("PLAYER","CLUB"))

cat("Players after join:", nrow(player_data), "\n")

#2) Derived metrics, prelim role for imputation, final role (single pass)
player_data <- player_data |>
  mutate(
    RUNS_PER_INNS    = ifelse(coalesce(BAT_INNS,0) > 0, coalesce(BAT_RUNS,0)/BAT_INNS, 0),
    WICKETS_PER_OVER = ifelse(coalesce(BOWL_OVERS,0) > 0, coalesce(BOWL_WKTS,0)/BOWL_OVERS, 0),
    DISMISSALS       = coalesce(TOTAL_VICTIMS, 0),
    bat_load         = coalesce(BAT_INNS, 0),
    bowl_load        = coalesce(BOWL_OVERS, 0)
  )

#thresholds (ONE source of truth)
MIN_STUMP        <- 2
MIN_KEEP_CATCHES <- 3
BAT_INNS_MIN     <- 4
BOWL_OVERS_MIN   <- 12     

#role gates
BAT_INNS_BAT     <- 4
BOWL_OVERS_BOW   <- 18     
AR_INNS          <- 4      
AR_OVERS         <- 16     
TH_STRONG        <- 0.25
TH_AR            <- 0.27   
MARGIN           <- 0.05

#PRELIM role 
player_data <- player_data |>
  mutate(
    ROLE_PRE = dplyr::case_when(
      coalesce(KEEP_STUMPINGS,0) >= MIN_STUMP | coalesce(KEEP_CATCHES,0) >= MIN_KEEP_CATCHES ~ "Wicketkeeper",
      bat_load >= 6 & bowl_load >= 20                                                         ~ "All-Rounder",
      bowl_load >= 15                                                                         ~ "Bowler",
      bat_load  >= 4                                                                          ~ "Batter",
      TRUE                                                                                    ~ "Unclassified"
    )
  )

#Impute by ROLE_PRE 
impute_by_role <- function(df, cols, role_col = "ROLE_PRE") {
  df %>%
    dplyr::group_by(.data[[role_col]]) %>%
    dplyr::mutate(across(all_of(cols), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))) %>%
    dplyr::ungroup()
}
player_data <- impute_by_role(player_data, c("BAT_SR", "BAT_AVG", "BOWL_ECON"), role_col = "ROLE_PRE")
