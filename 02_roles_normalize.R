#3) Normalise core metrics
player_data <- player_data |>
  mutate(
    NORM_RUNS     = minmax(RUNS_PER_INNS),
    NORM_STRIKE   = minmax(BAT_SR),
    NORM_AVG      = minmax(BAT_AVG),
    NORM_WICKETS  = minmax(WICKETS_PER_OVER),
    NORM_ECONOMY  = 1 - minmax(BOWL_ECON),  #higher is better
    NORM_FIELDING = minmax(DISMISSALS),
    
    #strengths from normalised metrics
    bat_strength  = rowMeans(cbind(NORM_RUNS, NORM_STRIKE, NORM_AVG), na.rm = TRUE),
    bowl_strength = rowMeans(cbind(NORM_WICKETS, NORM_ECONOMY),       na.rm = TRUE),
    share_bat     = ifelse(bat_load + bowl_load > 0, bat_load/(bat_load + bowl_load), NA_real_)
  ) |>
  mutate(
    ROLE = dplyr::case_when(
      #Keepers first
      coalesce(KEEP_STUMPINGS,0) >= MIN_STUMP | coalesce(KEEP_CATCHES,0) >= MIN_KEEP_CATCHES ~ "Wicketkeeper",
      
      #All-rounders
      bat_strength >= TH_AR & bowl_strength >= TH_AR &
        bat_load >= AR_INNS & bowl_load >= AR_OVERS &
        (bat_strength - bowl_strength) >= MARGIN                                            ~ "Batting All-Rounder",
      
      bat_strength >= TH_AR & bowl_strength >= TH_AR &
        bat_load >= AR_INNS & bowl_load >= AR_OVERS &
        (bowl_strength - bat_strength) >= MARGIN                                            ~ "Bowling All-Rounder",
      
      bat_strength >= TH_AR & bowl_strength >= TH_AR &
        bat_load >= AR_INNS & bowl_load >= AR_OVERS &
        abs(bat_strength - bowl_strength) < MARGIN & !is.na(share_bat) &
        share_bat >= 0.5                                                                     ~ "Batting All-Rounder",
      
      bat_strength >= TH_AR & bowl_strength >= TH_AR &
        bat_load >= AR_INNS & bowl_load >= AR_OVERS &
        abs(bat_strength - bowl_strength) < MARGIN                                          ~ "Bowling All-Rounder",
      
      #Specialists
      bowl_load >= BOWL_OVERS_BOW & (bowl_strength >= TH_STRONG | (bowl_strength - bat_strength) >= MARGIN) ~ "Bowler",
      bat_load  >= BAT_INNS_BAT   & (bat_strength  >= TH_STRONG | (bat_strength  - bowl_strength) >= MARGIN) ~ "Batter",
      
      #Relaxed fallback 1 
      bowl_load >= 12 & bowl_strength >= 0.22 ~ "Bowler",       
      bat_load  >= 3  & bat_strength  >= 0.22 ~ "Batter",
      
      #Relaxed fallback 2 
      bat_load >= 4 & bowl_load >= 8 & bat_strength >= bowl_strength ~ "Batting All-Rounder",
      bat_load >= 4 & bowl_load >= 8                                  ~ "Bowling All-Rounder",
      
      
      TRUE ~ "Unclassified"
    )
  )

cat("\nRole counts (FINAL):\n"); print(table(player_data$ROLE))
print(
  ggplot(player_data |> filter(!is.na(ROLE)), aes(x = ROLE)) +
    geom_bar(fill = "steelblue") +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
    labs(title = "Player Count by Role (final)", x = "Role", y = "Count") +
    theme_minimal()
)

#histograms (EDA)
op <- par(mfrow = c(2,3))
hist(player_data$NORM_RUNS,     main="Normalised Runs/Inn", xlab="NORM_RUNS")
hist(player_data$NORM_STRIKE,   main="Normalised Strike Rate", xlab="NORM_STRIKE")
hist(player_data$NORM_AVG,      main="Normalised Batting Avg", xlab="NORM_AVG")
hist(player_data$NORM_WICKETS,  main="Normalised Wickets/Over", xlab="NORM_WICKETS")
hist(player_data$NORM_ECONOMY,  main="Normalised Economy (↑ better)", xlab="NORM_ECONOMY")
hist(player_data$NORM_FIELDING, main="Normalised Fielding", xlab="NORM_FIELDING")
par(op)
