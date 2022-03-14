library(shiny)
library(shinyWidgets)
library(pracma)
library(plotly)
library(tidyverse)


# devtools::load_all("R/")
map_vis_list <- list(
  map19 = readRDS("data/map19_vis.Rds"),
  map24 = readRDS("data/map24_vis.Rds")
)
default_map <- "map24"
default_plot <- "VAF"

ui <- fluidPage(
  titlePanel("Bladder Cancer Evolution visualization"),
  sidebarLayout(

    sidebarPanel(
      radioButtons("map_choice",
        label = "Map choice:",
        choices = c(Map19 = "map19", Map24 = "map24"),
        selected = default_map
      ),
      radioButtons("plot_choice",
        label = "Plot choice:",
        choices = c(VAF = "VAF", `a/b` = "aob", `a/b hist` = "aob_hist"),
        selected = default_plot
      ),
      checkboxInput("plot_interactive",
        label = "Show interactive plot?:",
        value = FALSE
      ),
      sliderInput("time",
        label = "Show tumor at time:",
        min = 1,
        max = 250,
        value = 250
      ),
      sliderInput("VAF_treshold",
        label = "low VAF filter",
        min = 0,
        max = 0.1,
        value = c(0.01),
        step = 0.01
      ),
      selectizeInput("mutations_list",
        label = "Select mutations to show",
        choices = map_vis_list[[default_map]]$mutation_params$mutation_id,
        multiple = TRUE,
        options = list(create = TRUE)
      ),
      checkboxInput("select_all",
        label = "Select All",
        value = TRUE
      )
    ),

    mainPanel(
      plotOutput("plot", height = "650px"),
      plotlyOutput("plotly_plot", height = "650px"),
      tableOutput("table")
    )
  )
)


server <- function(input, output) {

  rv <- reactiveValues(mv = map_vis_list$map24)

  observeEvent(input$map_choice, {
    rv$mv <- map_vis_list[[input$map_choice]]
    updateSelectizeInput(getDefaultReactiveDomain(), "mutations_list",
      choices = rv$mv$mutation_params$mutation_id
    )
  })

  mut_ids <- reactive({
    if (input$select_all)
      rv$mv$mutation_params$mutation_id
    else
      input$mutations_list
  })

  output$plot <- renderPlot({
    legend_pos <- if (input$select_all) "none" else "right"

    plot_fun <-
      if (input$plot_choice == "VAF") plot_VAFs_2D_points
      else if (input$plot_choice == "aob") plot_aob_2D_points
      else  plot_aob_2D_histogram

    if (length(mut_ids()) > 0) {
      plot_fun(rv$mv,
        mut_ids(),
        t = rv$mv$times[input$time],
        VAF_treshold = input$VAF_treshold
      ) +
      theme(legend.position = legend_pos)
    }
  })

  output$plotly_plot <- renderPlotly({

    plot_fun <- if (input$plot_choice == "VAF")
      plot_VAFs_2D_points_interactive
    else
      plot_aob_2D_points_interactive

    if (length(mut_ids()) > 0 && input$plot_interactive)
      plot_fun(rv$mv,
        mut_ids(),
        t = rv$mv$times[input$time],
        VAF_treshold = input$VAF_treshold
      )
  })

  output$table <- renderTable({
    rv$mv$mutation_params %>%
      filter(mutation_id %in% mut_ids())
  })
}


shinyApp(ui = ui, server = server)
