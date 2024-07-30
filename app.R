library(shiny)
library(shinyFiles)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(rtracklayer)
library(ggplotlyExtra)
library(scales)
library(RColorBrewer)
library(plotly)
library(transPlotR)
library(viridis)
library(data.table)
library(shinythemes)


source("scripts/read_and_combine_gtf.R")
source("scripts/get_centered_df_by_gene.R")
source("scripts/get_centered_df_by_product.R")
source("scripts/create_contig_boundaries.R")
source("scripts/filter_by_coordinates.R")
source("scripts/plot_gtf.R")
source("scripts/sigmotize.R")
source("scripts/tanhize.R")
source("scripts/plot_preview.R")
source("scripts/concatenate_contigs.R")

options(shiny.maxRequestSize = 100 * 1024^2)  # Set maximum upload size

ui <- fluidPage(
  theme = shinytheme("flatly"),  # Установка темы
  titlePanel(
    div(
      h1("Syntenome"),
      h4("Shiny App for GTF Data Analysis"),
      style = "text-align: left;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Data Options"),
      shinyDirButton("folder", "Select Folder", "Please select a folder with GTF files"),
      actionButton("use_test_data", "Use Test Data"),
      hr(),
      h4("Plot Options"),
      radioButtons("center_by", "Center by:",
                   choices = list("None" = "none", "Gene" = "gene", "Product" = "product"),
                   selected = "none"),
      uiOutput("center_name_ui"),
      checkboxInput("filter_coordinates", "Filter by Coordinates", value = TRUE),
      uiOutput("coord_slider_ui"),
      checkboxInput("use_tanhize", "Use tanh transformation", value = TRUE),
      radioButtons("color_by", "Color By:",
                   choices = list("Gene" = "gene", "Product" = "product", "Gene Product" = "gene_product"),
                   selected = "gene")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Preview", plotlyOutput("previewPlot", height = "600px")),
        tabPanel("Main View", plotlyOutput("mainPlot", height = "600px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  roots <- if (.Platform$OS.type == "windows") {
    setNames(
      c(
        normalizePath("~"),  # Home directory
        normalizePath("."),  # Current working directory
        paste0(LETTERS, ":/")  # Drives from A: to Z:
      ),
      c("home", "wd", paste0(LETTERS, ":"))
    )
  } else {
    c(home = normalizePath("~"), wd = normalizePath("."), root = "/")
  }
  
  shinyDirChoose(input, "folder", roots = roots)
  
  selected_dir <- reactive({
    parseDirPath(roots = roots, input$folder)
  })
  
  gtf_data <- reactiveVal(NULL)
  
  observeEvent(selected_dir(), {
    req(selected_dir())
    gtf_data(read_and_combine_gtf(selected_dir()))
  })
  
  observeEvent(input$use_test_data, {
    test_data_dir <- "test_data"  # Specify the path to your test data
    gtf_data(read_and_combine_gtf(test_data_dir))
  })
  
  output$previewPlot <- renderPlotly({
    req(gtf_data())
    plot <- gtf_data() %>%
      concatenate_contigs() %>%
      create_contig_boundaries() %>%
      mutate(
        strain = as.factor(strain)
      ) %>%
      plot_preview()  # Returns a ggplotly object
    plot  # Return the plotly object directly
  })
  
  output$center_name_ui <- renderUI({
    if (input$center_by != "none") {
      textInput("center_name", "Enter Gene/Product Name:", value = if (input$center_by == "gene") "metG" else "alkaline phosphatase D family protein")
    }
  })
  
  coord_range <- reactive({
    req(gtf_data())
    data <- gtf_data() %>%
      concatenate_contigs()
    
    if (input$center_by == "gene" && input$center_name != "") {
      data <- data %>% get_centered_df_by_gene(input$center_name)
    } else if (input$center_by == "product" && input$center_name != "") {
      data <- data %>% get_centered_df_by_product(input$center_name)
    }
    
    c(min(data$start, na.rm = TRUE), max(data$end, na.rm = TRUE))
  })
  
  output$coord_slider_ui <- renderUI({
    req(coord_range())
    min_val <- coord_range()[1]
    max_val <- coord_range()[2]
    
    sliderInput("coord_range", "Coordinate Range:",
                min = if (input$center_by != "none") -abs(max(abs(min_val), abs(max_val))) else min_val,
                max = if (input$center_by != "none") abs(max(abs(min_val), abs(max_val))) else max_val,
                value = if (input$center_by != "none") c(-5000, 5000) else c(min_val, max_val))
  })
  
  main_plot_data <- reactive({
    req(gtf_data())
    
    data <- gtf_data() %>%
      concatenate_contigs()
    
    if (input$center_by == "gene") {
      req(input$center_name)
      if (input$center_name == "") {
        return(NULL)  # Return NULL if center name is not provided
      }
      data <- data %>% get_centered_df_by_gene(input$center_name)
    } else if (input$center_by == "product") {
      req(input$center_name)
      if (input$center_name == "") {
        return(NULL)  # Return NULL if center name is not provided
      }
      data <- data %>% get_centered_df_by_product(input$center_name)
    }
    
    if (input$filter_coordinates) {
      req(input$coord_range)
      data <- data %>% filter_by_coordinates(input$coord_range[1], input$coord_range[2])
    }
    
    if (input$use_tanhize) {
      data <- data %>% tanhize()
    }
    
    data <- data %>% mutate(strain = as.factor(strain))
    
    data
  })
  
  output$mainPlot <- renderPlotly({
    req(main_plot_data())
    plot <- main_plot_data() %>%
      plot_gtf(input$color_by)
    plot  # Return the plotly object directly
  })
}

shinyApp(ui = ui, server = server)