library(shiny)
library(rgl)

Filters <- matrix(c("3d data", "Filtered_*.csv"), 2, 2, byrow = TRUE)

shinyUI(fluidPage(
  titlePanel("Trackit 3D data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("trackitFile", "Choose Postprocessed and splined CSV File",
                multiple = FALSE,
                accept = c("csv", "comma-separated-values", ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      numericInput("startTime", "Start time:", 0, min = 0),
      numericInput("endTime", "End time:", 6000, min = 0),
      numericInput("minObjectNumber", "Smallest object number to be plotted:", 0, min = 0),
      numericInput("maxObjectNumber", "Largest object number to be plotted:", 1000, min = 0),
      h4(span(htmlOutput("warnings")), style="color:red"),
      actionButton("clearMessageButton", "Clear messages")
    ),
    mainPanel(
      #tableOutput("contents")
      rglwidgetOutput('rglWindow', width = '1024px', height = '1024px')
    )
  )
))

