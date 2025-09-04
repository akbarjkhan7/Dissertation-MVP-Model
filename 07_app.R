#Shiny App (dashboard showing more visuals)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(dplyr)
  library(ggplot2)
  library(fmsb)     
  library(scales)   
})

#Run pipeline
if (!exists("player_data")) {
  stop("Shiny: `player_data` not found. Please run the MVP pipeline script first (so `player_data` is created).")
}

#Build dashboard dataset from output
mvp_dash <- player_data

#Role factor
if (!is.null(mvp_dash$ROLE)) {
  role_levels <- c("Batter",
                   "Batting All-Rounder",
                   "Bowler",
                   "Bowling All-Rounder",
                   "Wicketkeeper",
                   "Unclassified")
  if (any(mvp_dash$ROLE == "All-Rounder", na.rm = TRUE)) {
    role_levels <- c(role_levels, "All-Rounder")
  }
  mvp_dash$ROLE <- factor(mvp_dash$ROLE, levels = role_levels)
}

#Metrics used on the radar
metric_cols <- c("NORM_RUNS","NORM_STRIKE","NORM_AVG","NORM_WICKETS","NORM_ECONOMY","NORM_FIELDING")

#Ensure metric columns exist and are numeric
missing_metrics <- setdiff(metric_cols, names(mvp_dash))
if (length(missing_metrics)) mvp_dash[missing_metrics] <- NA_real_
for (cc in metric_cols) {
  mvp_dash[[cc]] <- suppressWarnings(as.numeric(mvp_dash[[cc]]))
}

#Coerce a few display fields
display_cols <- c("MVP_SCORE","RUNS_PER_INNS","BAT_SR","BAT_AVG","WICKETS_PER_OVER","BOWL_ECON")
for (cc in intersect(display_cols, names(mvp_dash))) {
  mvp_dash[[cc]] <- suppressWarnings(as.numeric(mvp_dash[[cc]]))
}

#UI
ui <- dashboardPage(
  dashboardHeader(title = "NEDCL MVP Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Leaderboard",      tabName = "leaderboard", icon = icon("list-ol")),
      menuItem("Compare Players",  tabName = "compare",     icon = icon("user-friends")),
      menuItem("Club Performance", tabName = "clubs",       icon = icon("building"))
    )
  ),
  dashboardBody(
    tabItems(
      
      #Leaderboard
      tabItem(tabName = "leaderboard",
              fluidRow(
                box(title = "Filters", width = 4, solidHeader = TRUE, status = "primary",
                    selectInput(
                      "role", "Role:",
                      choices  = levels(mvp_dash$ROLE)[levels(mvp_dash$ROLE) %in% unique(mvp_dash$ROLE)],
                      selected = if ("Batter" %in% mvp_dash$ROLE) "Batter"
                      else as.character(head(stats::na.omit(mvp_dash$ROLE), 1))
                    ),
                    selectInput(
                      "club", "Club:",
                      choices  = c("All", sort(unique(stats::na.omit(mvp_dash$CLUB)))),
                      selected = "All"
                    ),
                    sliderInput("n_top", "Show Top N:", min = 5, max = 20, value = 10, step = 1)
                ),
                
                box(title = "Top Players", width = 5, solidHeader = TRUE, status = "primary",
                    helpText("Tip: click a row to open the player profile."),
                    DTOutput("tbl_top"),
                    br(),
                    plotOutput("plot_top", height = 300)
                ),
                
                box(title = "Player Profile", width = 3, solidHeader = TRUE, status = "info",
                    uiOutput("player_header"),
                    div(style="margin-top:8px"),
                    fluidRow(valueBoxOutput("vb_mvp",  width = 12)),
                    fluidRow(
                      valueBoxOutput("vb_runs", width = 6),
                      valueBoxOutput("vb_sr",   width = 6)
                    ),
                    fluidRow(
                      valueBoxOutput("vb_wkts", width = 6),
                      valueBoxOutput("vb_econ", width = 6)
                    ),
                    actionButton("btn_more", "Open detailed profile", class = "btn btn-primary btn-block")
                )
              ),
              
              fluidRow(
                box(title = "Player Radar (same selection)", width = 12, solidHeader = TRUE, status = "warning",
                    plotOutput("plot_radar_inline", height = 320)
                )
              )
      ),
      
      #Compare
      tabItem(tabName = "compare",
              fluidRow(
                box(title = "Choose Role & Players", width = 4, solidHeader = TRUE, status = "info",
                    selectInput(
                      "cmp_role", "Role to compare:",
                      choices  = levels(mvp_dash$ROLE),
                      selected = if ("Batter" %in% levels(mvp_dash$ROLE)) "Batter" else levels(mvp_dash$ROLE)[1]
                    ),
                    uiOutput("cmp_player_inputs"),
                    helpText("You can only compare two players within the same role.")
                ),
                box(title = "Radar Comparison (same role)", width = 8, solidHeader = TRUE, status = "info",
                    uiOutput("cmp_warning"),
                    plotOutput("plot_cmp_radar", height = 520)
                )
              )
      ),
      
      #Clubs
      tabItem(tabName = "clubs",
              fluidRow(
                box(title = "Average MVP by Club", width = 12, solidHeader = TRUE, status = "success",
                    plotOutput("plot_club", height = 520)
                )
              )
      )
    ),
    
    #Hidden plot outputs for the modal
    div(style = "display:none;",
        plotOutput("modal_heatmap", height = 1),
        plotOutput("modal_radar",   height = 1)
    )
  )
)

#Server 
server <- function(input, output, session) {
  
  fmt_num <- function(x, digits = 2) {
    x <- suppressWarnings(as.numeric(x))
    ifelse(is.finite(x), format(round(x, digits), nsmall = digits), "—")
  }
  
  #Leaderboard
  top_filtered <- reactive({
    df <- mvp_dash
    if (!is.null(input$role) && input$role %in% df$ROLE) df <- dplyr::filter(df, ROLE == input$role)
    if (!is.null(input$club) && input$club != "All")      df <- dplyr::filter(df, CLUB == input$club)
    df %>%
      dplyr::filter(!is.na(MVP_SCORE)) %>%
      dplyr::arrange(dplyr::desc(MVP_SCORE)) %>%
      dplyr::slice_head(n = input$n_top)
  })
  
  output$tbl_top <- renderDT({
    datatable(
      top_filtered()[, intersect(c("PLAYER","CLUB","ROLE","MVP_SCORE"), names(mvp_dash)), drop = FALSE],
      selection = "single", rownames = FALSE,
      options = list(pageLength = 10, dom = "tip", order = list(list(3, "desc")))
    )
  })
  
  selected_player <- reactiveVal(NULL)
  observeEvent(input$tbl_top_rows_selected, {
    idx <- input$tbl_top_rows_selected
    if (length(idx)) selected_player(top_filtered()[idx, , drop = FALSE]$PLAYER)
  }, ignoreInit = TRUE)
  observe({
    if (is.null(selected_player()) && nrow(top_filtered()) > 0) {
      selected_player(top_filtered()$PLAYER[1])
    }
  })
  
  player_row <- reactive({
    req(selected_player())
    mvp_dash %>% dplyr::filter(PLAYER == selected_player()) %>% dplyr::slice(1)
  })
  
  output$plot_top <- renderPlot({
    df <- top_filtered()
    ggplot(df, aes(x = reorder(PLAYER, MVP_SCORE), y = MVP_SCORE, fill = CLUB)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Top", input$n_top, "—", as.character(input$role)),
           x = "Player", y = "MVP Score") +
      theme_minimal()
  })
  
  output$player_header <- renderUI({
    r <- player_row()
    tagList(
      h4(strong(r$PLAYER)),
      div(style="font-size:13px;color:#666;", paste0("Club: ", r$CLUB, "  |  Role: ", r$ROLE))
    )
  })
  output$vb_mvp  <- renderValueBox({ r <- player_row(); valueBox(fmt_num(r$MVP_SCORE, 3),        "MVP score",  icon = icon("star"),            color = "purple") })
  output$vb_runs <- renderValueBox({ r <- player_row(); valueBox(fmt_num(r$RUNS_PER_INNS, 2),    "Runs / Inn", icon = icon("chart-line"),      color = "teal") })
  output$vb_sr   <- renderValueBox({ r <- player_row(); valueBox(fmt_num(r$BAT_SR, 1),           "Strike rate",icon = icon("bolt"),            color = "teal") })
  output$vb_wkts <- renderValueBox({ r <- player_row(); valueBox(fmt_num(r$WICKETS_PER_OVER, 3), "Wkts / Over",icon = icon("bullseye"),        color = "olive") })
  output$vb_econ <- renderValueBox({ r <- player_row(); valueBox(fmt_num(r$BOWL_ECON, 2),        "Economy",    icon = icon("shield-halved"),   color = "olive") })
  
  #Inline radar for selected player
  output$plot_radar_inline <- renderPlot({
    r <- player_row()
    if (!all(metric_cols %in% names(r))) return(invisible(NULL))
    dfm <- r[, metric_cols, drop = FALSE]
    dfm[] <- lapply(dfm, function(z){ z <- suppressWarnings(as.numeric(z)); z[!is.finite(z)] <- 0; z })
    
    rad <- rbind(rep(1, length(metric_cols)), rep(0, length(metric_cols)), dfm)
    rad <- as.data.frame(rad, check.names = FALSE)
    colnames(rad) <- c("Runs","StrikeRate","Average","Wkts/Over","Economy(↑good)","Fielding")
    
    fmsb::radarchart(
      rad, axistype = 1,
      pcol  = "#1f77b4",
      pfcol = scales::alpha("#1f77b4", 0.30),
      plwd  = 2,
      cglcol = "grey70", cglty = 1, axislabcol = "grey40", vlcex = 0.9,
      title = paste("Profile:", r$PLAYER)
    )
  })
  
  #Hidden modal plots
  output$modal_heatmap <- renderPlot({
    r  <- player_row()
    if (!all(metric_cols %in% names(r))) return(invisible(NULL))
    vv <- as.numeric(r[1, metric_cols]); vv[!is.finite(vv)] <- 0
    hm <- data.frame(
      Metric = factor(c("Runs/Inn","Strike rate","Batting avg","Wkts/Over","Economy (↑ good)","Fielding"),
                      levels = c("Runs/Inn","Strike rate","Batting avg","Wkts/Over","Economy (↑ good)","Fielding")),
      Value  = vv
    )
    ggplot(hm, aes(x = 1, y = Metric, fill = Value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.2f", Value)), size = 4) +
      scale_fill_gradient(limits = c(0,1), low = "#f0f0f0", high = "#2c7fb8", oob = squish) +
      labs(x = NULL, y = NULL, fill = "Norm") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), panel.grid = element_blank(), legend.position = "right")
  })
  output$modal_radar <- renderPlot({
    r <- player_row()
    if (!all(metric_cols %in% names(r))) return(invisible(NULL))
    dfm <- r[, metric_cols, drop = FALSE]
    dfm[] <- lapply(dfm, function(z){ z <- suppressWarnings(as.numeric(z)); z[!is.finite(z)] <- 0; z })
    
    rad <- rbind(rep(1, length(metric_cols)), rep(0, length(metric_cols)), dfm)
    rad <- as.data.frame(rad, check.names = FALSE)
    colnames(rad) <- c("Runs","StrikeRate","Average","Wkts/Over","Economy(↑good)","Fielding")
    
    fmsb::radarchart(
      rad, axistype = 1,
      pcol  = "#e15759",
      pfcol = scales::alpha("#e15759", 0.30),
      plwd  = 2,
      cglcol = "grey70", cglty = 1, axislabcol = "grey40", vlcex = 0.9,
      title = NULL
    )
  })
  observeEvent(input$btn_more, {
    showModal(modalDialog(
      size = "l",
      title = paste("Detailed profile:", player_row()$PLAYER),
      fluidRow(
        column(6, tags$h5("Normalised metrics (0–1)"), plotOutput("modal_heatmap", height = 280)),
        column(6, tags$h5("Radar"),                        plotOutput("modal_radar",   height = 280))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  #Compare tab (same-role & robust) 
  cmp_pool <- reactive({
    req(input$cmp_role)
    mvp_dash %>%
      dplyr::filter(!is.na(ROLE), ROLE == input$cmp_role) %>%
      dplyr::mutate(across(all_of(metric_cols), ~ { z <- suppressWarnings(as.numeric(.)); z }))
  })
  
  output$cmp_player_inputs <- renderUI({
    pool <- cmp_pool()
    choices <- sort(unique(pool$PLAYER))
    if (length(choices) < 2) {
      return(div(style="color:#a94442; margin-bottom:6px;",
                 "Not enough players in this role to compare."))
    }
    def <- pool %>%
      dplyr::filter(!is.na(MVP_SCORE)) %>%
      dplyr::arrange(dplyr::desc(MVP_SCORE)) %>%
      dplyr::pull(PLAYER) %>% unique()
    if (length(def) < 2) def <- choices[1:2]
    tagList(
      selectInput("p1", "Player 1:", choices = choices, selected = def[1]),
      selectInput("p2", "Player 2:", choices = choices, selected = def[2])
    )
  })
  
  output$cmp_warning <- renderUI({
    pool <- cmp_pool()
    if (is.null(input$p1) || is.null(input$p2)) {
      return(div(style="color:#a94442; margin-bottom:6px;", "Select two players from the chosen role."))
    }
    if (!(input$p1 %in% pool$PLAYER) || !(input$p2 %in% pool$PLAYER)) {
      return(div(style="color:#a94442; margin-bottom:6px;", "Both players must belong to the selected role."))
    }
    if (identical(input$p1, input$p2)) {
      return(div(style="color:#8a6d3b; margin-bottom:6px;", "You selected the same player twice."))
    }
    NULL
  })
  
  output$plot_cmp_radar <- renderPlot({
    pool <- cmp_pool()
    req(input$p1, input$p2, input$p1 != input$p2)
    
    #rows in fixed order (p1 then p2)
    df <- pool %>%
      dplyr::filter(PLAYER %in% c(input$p1, input$p2)) %>%
      dplyr::slice(match(c(input$p1, input$p2), PLAYER))
    validate(need(nrow(df) == 2, "Could not find both players in this role."))
    
    #numeric metrics + replace non-finite with 0
    dfm <- df[, metric_cols, drop = FALSE]
    dfm[] <- lapply(dfm, function(z){ z <- suppressWarnings(as.numeric(z)); z[!is.finite(z)] <- 0; z })
    
    #Require at least 3 finite metrics per player
    ok <- apply(dfm, 1, function(z) sum(is.finite(z)) >= 3)
    validate(need(all(ok), "Selected players do not have enough metrics to draw the radar."))
    
    M <- length(metric_cols)
    radar_df <- rbind(rep(1, M), rep(0, M), dfm)
    radar_df <- as.data.frame(radar_df, check.names = FALSE)
    colnames(radar_df) <- c("Runs","StrikeRate","Average","Wkts/Over","Economy(↑good)","Fielding")
    
    ns <- nrow(radar_df) - 2L  # number of players (should be 2)
    cols_line <- c("#1f77b4", "#e15759")[seq_len(ns)]
    cols_fill <- scales::alpha(cols_line, 0.30)
    
    fmsb::radarchart(
      radar_df, axistype = 1,
      pcol  = cols_line,
      pfcol = cols_fill,
      plwd  = rep(2, ns),
      cglcol = "grey50", cglty = 1, axislabcol = "grey70", vlcex = 0.9,
      title = paste(df$PLAYER[1], "vs", df$PLAYER[2], "—", as.character(input$cmp_role))
    )
    legend("topright", legend = df$PLAYER, col = cols_line, lty = 1, lwd = 2, bty = "n")
  }, bg = "white")
  
  #Clubs
  output$plot_club <- renderPlot({
    club_summary <- mvp_dash %>%
      dplyr::group_by(CLUB) %>%
      dplyr::summarise(Mean_MVP = mean(MVP_SCORE, na.rm = TRUE),
                       Players  = dplyr::n()) %>%
      dplyr::arrange(dplyr::desc(Mean_MVP))
    ggplot(club_summary, aes(x = reorder(CLUB, Mean_MVP), y = Mean_MVP, fill = Players)) +
      geom_col() +
      coord_flip() +
      labs(title = "Average MVP Score by Club", x = "Club", y = "Average MVP Score") +
      theme_minimal()
  })
}

#Run it
shinyApp(ui, server)