library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(RColorBrewer)
library(tidyverse)

# UI
ui <- fluidPage(
  titlePanel("KITE Model Simplified", windowTitle = "KITE Model"),
  h3("Decoupling Europe"),
  p("Increase trade barriers between EU and different trade partners."),
  sidebarLayout(
    sidebarPanel(
      selectInput("enemy", "EU Decouples From:", choices = c("USA", "CHN", "IND", "RUS", "BRA")),
      selectInput("tradeFlow", "Select Decoupling Flow:", choices = c("Imports", "Exports", "Imports & Exports")),
      sliderInput("tradeBarrier", "Select Trade Barrier Increase:",
                  min = 1, max = 4, value = 1, step = 1,
                  ticks = FALSE),  # Optional: disable default ticks for cleaner UI
      tags$div(style = "display:flex; justify-content:space-between;",
               tags$span("10%"),
               tags$span("25%"),
               tags$span("50%"),
               tags$span("100%")
               ),
      tags$br(),  # Add space
      downloadButton("downloadData", "Download Data")
      ),

    mainPanel(
      # display table
      # tableOutput("results"),
      p("Welfare Change in %", align = "center"),
      leafletOutput("worldMap"),
    )
  ),

  # Footer
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

# Server
server <- function(input, output, session) {
  # Reactive expression to map slider values to percentage increases
  tradeBarrierValue <- reactive({
    switch(as.character(input$tradeBarrier),
           "1" = 1.1,
           "2" = 1.25,
           "3" = 1.5,
           "4" = 2)
  })

  # Reactive expression to map trade flow input to data frame values
  tradeFlowMap <- reactive({
    switch(input$tradeFlow,
           "Imports" = "imports",
           "Exports" = "exports",
           "Imports & Exports" = "imports_and_exports")
  })

  dataset <- reactive({
    data <- read_csv("decopuling_results.csv")
    # Filter and sort data
    filtered <- data %>%
      filter(ntb == tradeBarrierValue(),  # Use tradeBarrierValue as a function
             enemy == input$enemy,
             scenario == tradeFlowMap()) %>%
      select(-ntb, -enemy, -scenario) %>%
      rename(welfare_change = value) %>%
      arrange(desc(welfare_change))

    return(filtered)
  })

  # Reactive expression to generate the map data
  mapData <- reactive({
    map <- read_rds("map_world_gtap.rds")
    data <- fread("decopuling_results.csv")
    mergedData <- merge(
      map,
      data[enemy == input$enemy & ntb == tradeBarrierValue() & scenario == tradeFlowMap(), .(value, gtap_code = country)],
      by = "gtap_code"
    )
    return(mergedData)
  })

  # output$results <- renderTable({
  #   dataset()
  # })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_to_download <- dataset()  # Get the current filtered dataset
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )

  # Reactive expression for color scale
  colorScale <- reactive({
    data <- mapData()  # Ensure mapData is available
    if (nrow(data) == 0) {
      return(NULL)
    }

    # Calculate max absolute value for symmetric color scaling
    max_abs_val <- max(abs(range(data$value, na.rm = TRUE)), na.rm = TRUE)

    # Create a color palette function
    paletteFunc <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))

    # Create color scale function
    function(val) {
      paletteFunc(100)[findInterval(val, seq(-max_abs_val, max_abs_val, length.out = 100))]
    }
  })

  output$worldMap <- renderLeaflet({
    mapData <- mapData()  # Get the reactive map data

    if (is.null(colorScale())) {
      return(NULL)
    }

    leaflet(mapData) %>%
      setView(lng = 10, lat = 30, zoom = 1) %>%  # You can adjust the lat, lng, and zoom as needed
      addTiles() %>%  # Add default OpenStreetMap tiles
      addPolygons(
        fillColor = ~colorScale()(value),  # Color countries based on value
        # fillColor = ~colorQuantile("YlOrRd", value)(value),  # Color countries based on value
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(gtap_code, ": ", round(value, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
}


# Run the application
shinyApp(ui = ui, server = server)

