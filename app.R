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
library(shinythemes)

source("scripts/get_centered_df_by_gene.R")
source("scripts/get_centered_df_by_product.R")
source("scripts/create_contig_boundaries.R")
source("scripts/filter_by_coordinates.R")
source("scripts/plot_gtf.R")
source("scripts/sigmotize.R")
source("scripts/tanhize.R")
source("scripts/plot_preview.R")
source("scripts/concatenate_contigs.R")

options(shiny.maxRequestSize = 30 * 1024^2)  # Set maximum upload size

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(
    div(
      h1("Syntenome"),
      h4("Shiny App for GTF Data Analysis"),
      style = "text-align: left;"
    )
  ),
  tabsetPanel(
    tabPanel("Data & Preview",
             sidebarLayout(
               sidebarPanel(
                 h4("Data Options"),
                 fileInput("gtf_files", "Choose GTF Files", multiple = TRUE, accept = ".gtf"),
                 hr(),
                 h4("Selected Files"),
                 tableOutput("file_list"),  # Вывод списка файлов
                 actionButton("load_files", "LOAD"),  # Кнопка LOAD теперь под списком файлов
                 actionButton("use_test_data", "Use Test Data"),
                 actionButton("remove_selected", "Remove Selected Files")
               ),
               mainPanel(
                 plotlyOutput("previewPlot", height = "600px")
               )
             )
    ),
    tabPanel("Plot Options & Main View",
             sidebarLayout(
               sidebarPanel(
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
                              selected = "gene"),
                 checkboxInput("show_legend", "Show Legend", value = TRUE),
                 uiOutput("strain_select_ui")
               ),
               mainPanel(
                 plotlyOutput("mainPlot", height = "600px")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  gtf_data <- reactiveVal(NULL)
  selected_files <- reactiveVal(data.frame(name = character(), datapath = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$gtf_files, {
    req(input$gtf_files)
    
    # Добавляем новые файлы к текущему списку сразу после их выбора
    new_files <- data.frame(name = input$gtf_files$name, datapath = input$gtf_files$datapath, stringsAsFactors = FALSE)
    updated_files <- rbind(selected_files(), new_files)
    
    # Обновляем список файлов
    selected_files(updated_files)
  })
  
  observeEvent(input$remove_selected, {
    req(input$selected_files)
    
    # Удаляем выбранные файлы из списка
    remaining_files <- selected_files()[!selected_files()$name %in% input$selected_files, ]
    selected_files(remaining_files)
  })
  
  observeEvent(input$load_files, {
    req(input$selected_files)
    
    # Загружаем только выбранные файлы
    files_to_load <- selected_files() %>% filter(name %in% input$selected_files)
    
    combined_gtf <- map_df(seq_len(nrow(files_to_load)), function(i) {
      file_path <- files_to_load$datapath[i]
      strain <- sub("\\.gtf$", "", files_to_load$name[i])
      
      # Импортируем GTF-файл и добавляем столбец strain
      gtf_data <- import(file_path)
      gtf_data <- as_tibble(gtf_data) %>% 
        mutate(strain = strain)
      
      return(gtf_data)
    })
    
    # Фильтруем только записи типа CDS и добавляем столбец gene_product
    combined_gtf <- combined_gtf %>% 
      filter(type == "CDS") %>%
      mutate(gene_product = paste(gene, product, sep = "|"))
    
    gtf_data(combined_gtf)
  })
  
  observeEvent(input$use_test_data, {
    test_data_dir <- "./test_data"  # Путь к тестовым данным
    
    # Получение списка файлов GTF в папке
    test_files <- list.files(path = test_data_dir, pattern = "\\.gtf$", full.names = TRUE)
    
    # Извлечение имен файлов для отображения
    test_file_names <- basename(test_files)
    
    # Добавление тестовых данных к выбранным файлам
    selected_files(rbind(
      selected_files(), 
      data.frame(name = test_file_names, datapath = test_files, stringsAsFactors = FALSE)
    ))
  })
  
  output$file_list <- renderUI({
    checkboxGroupInput("selected_files", "", 
                       choices = selected_files()$name,
                       selected = selected_files()$name)  # По умолчанию выбираем все файлы
  })
  
  output$previewPlot <- renderPlotly({
    req(gtf_data())
    plot <- gtf_data() %>%
      concatenate_contigs() %>%
      create_contig_boundaries() %>%
      mutate(
        strain = as.factor(strain)
      ) %>%
      plot_preview()
    plot
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
  
  output$strain_select_ui <- renderUI({
    req(gtf_data())
    data <- gtf_data()
    selectInput("selected_strains", "Select Strains to Invert X Axis:", 
                choices = levels(as.factor(data$strain)), multiple = TRUE)
  })
  
  main_plot_data <- reactive({
    req(gtf_data())
    
    data <- gtf_data() %>%
      concatenate_contigs()
    
    if (input$center_by == "gene") {
      req(input$center_name)
      if (input$center_name == "") {
        return(NULL)
      }
      data <- data %>% get_centered_df_by_gene(input$center_name)
    } else if (input$center_by == "product") {
      req(input$center_name)
      if (input$center_name == "") {
        return(NULL)
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
      plot_gtf(fill_by = input$color_by, show_legend = input$show_legend, invert_strains = input$selected_strains)
    plot
  })
}

shinyApp(ui = ui, server = server)