# app.R  —  Trump Tariff Simulator (KITE)
library(shiny)
library(shinyWidgets)   # << NEW for sliderTextInput + updateSliderTextInput
library(leaflet)
library(sf)
library(data.table)
library(arrow)
library(RColorBrewer)
library(tidyverse)
library(memoise)

FOCUS <- c("CAN","CHN","MEX","JPN","EU27")

# --- Load keys & map ---
scen  <- read_parquet("scenario_key.parquet") |> as.data.table()
ckey  <- fread("country_key.csv")             |> setkey(country_id)
world <- read_rds("map_world_gtap.rds") |> sf::st_transform(4326)

# standardize mapping key
if (!"gtap_code" %in% names(world) && "iso3c" %in% names(world)) world$gtap_code <- world$iso3c
if (!"gtap_code" %in% names(ckey)) ckey[, gtap_code := iso3c]

# remove XNA from map entirely  << NEW
if ("gtap_code" %in% names(world)) world <- subset(world, gtap_code != "XNA")
if ("gtap_code" %in% names(world)) world <- subset(world, gtap_code != "MNG")

# steps per slider (from scenario_key)
steps_by_country <- lapply(FOCUS, function(cn) sort(unique(scen[[cn]])))
names(steps_by_country) <- FOCUS
default_steps <- sapply(FOCUS, function(cn) min(steps_by_country[[cn]], na.rm = TRUE))

# find scenario_id from slider selections (named int vector)
find_scenario_id <- function(sel) {
  hit <- scen[ CAN  == sel[["CAN"]]  &
                 CHN  == sel[["CHN"]]  &
                 MEX  == sel[["MEX"]]  &
                 JPN  == sel[["JPN"]]  &
                 EU27 == sel[["EU27"]], scenario_id]
  if (length(hit)) hit[[1]] else NA_integer_
}

# read one metric (INT-ENCODED deviations): welfare/export/production are int32 encodings
.read_metric <- function(sid, metric_col) {
  cols <- c("scenario_id","country_id", metric_col)
  dt <- read_parquet("facts.parquet", col_select = cols) |> as.data.table()
  dt <- dt[scenario_id == sid, .(country_id, enc = get(metric_col))]
  dt[, value := enc / 1000]                              # percent change directly
  dt <- ckey[dt, on = .(country_id), nomatch = 0L][, .(gtap_code, value)]
  dt <- dt[gtap_code != "XNA"]                           # << NEW: drop XNA in results too
  dt[]
}
read_metric <- memoise(.read_metric)

# column names stored in Parquet
pick_metric_col <- function(choice) switch(choice,
                                           welfare    = "welfare",
                                           exports     = "exports",
                                           production = "production"
)

# choices per country as CHARACTER (canonical)
choices_chr_list <- lapply(FOCUS, function(cn) as.character(sort(unique(scen[[cn]]))))
names(choices_chr_list) <- FOCUS

# --- Benchmarks as ratios (from NYT / your baseline) ---
benchmarks <- data.table::data.table(
  country = c("CAN","JPN","CHN","MEX","EU27"),
  value   = c(1.35 , 1.15, 1.30, 1.25, 1.15)
)

ratio_to_step <- function(r) as.integer(round((r - 1) * 100))  # 1.35 -> 35

bench_choice_chr <- sapply(FOCUS, function(cn) {
  choices_chr <- choices_chr_list[[cn]]
  choices_int <- as.integer(choices_chr)
  target_int  <- ratio_to_step(benchmarks[country == cn, value])
  choices_chr[ which.min(abs(choices_int - target_int)) ]
}, USE.NAMES = TRUE)

# ---------------- UI ----------------
ui <- fluidPage(
  titlePanel("Trump Tariff Simulator", windowTitle = "KITE Model"),
  p("Choose individual country-level tariffs on US imports."),

  sidebarLayout(
    sidebarPanel(
      h4("US tariffs on imports (percentage points)"),
      # 1) Discrete sliders with only allowed values  << NEW
      lapply(FOCUS, function(cn) {
        sliderTextInput(
          inputId  = paste0("s_", cn),
          label    = cn,
          choices  = choices_chr_list[[cn]],   # <-- canonical choices
          selected = bench_choice_chr[[cn]],   # <-- exact choice string
          grid     = TRUE,
          hide_min_max = TRUE,
          dragRange = FALSE
        )
      }),
      tags$br(),
      # 2) Reset button to defaults  << NEW
      actionButton("reset_defaults", "Current tariffs"),
      tags$br(), tags$br(),
      radioButtons("metric", "Output to display:",
                   choices = c("Welfare" = "welfare",
                               "Total exports" = "exports",
                               "Total production" = "production"),
                   inline = FALSE),
      tags$br(),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      p("Change in % relative to baseline", align = "center"),
      leafletOutput("worldMap"),
    )
  ),

  tags$footer(
    style = "margin-top: 20px; text-align: center; font-size: 0.8em;",
    "Developed by ",
    tags$a(href = "https://www.ifw-kiel.de/", target = "_blank", "Kiel Institute for the World Economy"),
    " and ",
    tags$a(href = "https://wifo.ac.at/en", target = "_blank", "Austrian Institute of Economic Research"),
    ". Code is available at ",
    tags$a(href = "https://github.com/julianhinz/KITE/tree/main", target = "_blank", "GitHub"),
    "."
  )
)

# --------------- Server ---------------
server <- function(input, output, session) {

  # 2) Reset to defaults when button pressed  << NEW
  observeEvent(input$reset_defaults, {
    for (cn in FOCUS) {
      shinyWidgets::updateSliderTextInput(
        session,
        inputId  = paste0("s_", cn),
        choices  = choices_chr_list[[cn]],        # <-- send choices too
        selected = bench_choice_chr[[cn]]         # <-- exact string in choices
      )
    }
  })

  # current selection (convert sliderTextInput strings -> integers)
  step_sel <- reactive({
    c(
      CAN  = as.integer(input$s_CAN),
      CHN  = as.integer(input$s_CHN),
      MEX  = as.integer(input$s_MEX),
      JPN  = as.integer(input$s_JPN),
      EU27 = as.integer(input$s_EU27)
    )
  })

  sid <- reactive(find_scenario_id(step_sel()))

  # table for download
  dataset <- reactive({
    req(!is.na(sid()))
    metric_col <- pick_metric_col(input$metric)
    dt <- read_metric(sid(), metric_col)  # gtap_code, value (%)
    setorder(dt, -value)
    setnames(dt, "value", "welfare_change")  # just to keep your old naming
    dt[]
  })

  output$downloadData <- downloadHandler(
    filename = function() paste0("kite_results_", Sys.Date(), ".csv"),
    content  = function(file) fwrite(dataset(), file)
  )

  # map data: merge sf + values (value is percent); XNA already removed
  mapData <- reactive({
    req(!is.na(sid()))
    metric_col <- pick_metric_col(input$metric)
    vals <- read_metric(sid(), metric_col)         # gtap_code, value
    mergedData <- merge(world, vals, by = "gtap_code", all.x = TRUE)
    mergedData
  })

  colorScale <- reactive({
    m <- mapData()
    if (nrow(m) == 0) return(NULL)
    max_abs_val <- max(abs(range(m$value, na.rm = TRUE)), na.rm = TRUE)
    paletteFunc <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))
    function(val) {
      paletteFunc(100)[findInterval(val, seq(-max_abs_val, max_abs_val, length.out = 100))]
    }
  })

  output$worldMap <- renderLeaflet({
    m <- mapData()
    if (is.null(colorScale())) return(NULL)
    lbl_metric <- switch(input$metric,
                         welfare = "Welfare", exports = "Total exports", production = "Total production")

    leaflet(m) %>%
      setView(lng = 10, lat = 30, zoom = 1) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorScale()(value),
        weight = 1, opacity = 1, color = "white", dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 2, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
        label = ~paste(gtap_code, ": ", lbl_metric, " ", sprintf("%.2f%%", value)),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px", direction = "auto")
      )
  })
}

shinyApp(ui, server)
