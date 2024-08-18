plot_gtf <- function(data, fill_by = "product", show_legend = TRUE, 
                     show_strand = TRUE, show_border = TRUE, 
                     invert_strains = NULL) {
  # Проверяем, существует ли указанный столбец в данных
  if (!fill_by %in% colnames(data)) {
    stop(paste("Столбец", fill_by, "не найден в данных"))
  }
  
  # Инвертируем координаты для указанных штаммов
  if (!is.null(invert_strains)) {
    data <- data %>%
      mutate(recalculated_start = ifelse(strain %in% invert_strains, -recalculated_start, recalculated_start),
             recalculated_end = ifelse(strain %in% invert_strains, -recalculated_end, recalculated_end))
  }
  
  # Создаем ggplot
  p <- ggplot(data) +
    aes(xmin = recalculated_start, xmax = recalculated_end, ymin = as.numeric(strain) - 0.4, ymax = as.numeric(strain) + 0.4, fill = .data[[fill_by]]) +
    geom_rect(
      color = if (show_border) "black" else NA,  # Использовать черный цвет, если show_border TRUE
      size = 0.2  # Толщина окантовки
    ) +
    scale_y_continuous(breaks = seq_along(levels(data$strain)), labels = levels(data$strain)) +
    theme_minimal() +
    labs(x = "Relative Coordinates", y = "Strain", fill = fill_by) +
    theme(legend.position = if (show_legend) "right" else "none")
  
  if (show_strand) {
    # Добавляем направляющие стрелки
    p <- p + geom_segment(
      data = data %>% filter(strand == "+"),
      aes(
        x = recalculated_start, xend = recalculated_end, 
        y = as.numeric(strain) - 0.4, yend = as.numeric(strain)
      ),
      arrow = arrow(type = "closed", length = unit(0.1, "cm")),
      color = "black",
      size = 0.3 # Уменьшаем толщину линии
    ) +
      geom_segment(
        data = data %>% filter(strand == "+"),
        aes(
          x = recalculated_start, xend = recalculated_end, 
          y = as.numeric(strain) + 0.4, yend = as.numeric(strain)
        ),
        arrow = arrow(type = "closed", length = unit(0.1, "cm")),
        color = "black",
        size = 0.3 # Уменьшаем толщину линии
      ) +
      geom_segment(
        data = data %>% filter(strand == "-"),
        aes(
          x = recalculated_start, xend = recalculated_end, 
          y = as.numeric(strain) + 0.4, yend = as.numeric(strain)
        ),
        arrow = arrow(type = "closed", length = unit(0.1, "cm")),
        color = "black",
        size = 0.3 # Уменьшаем толщину линии
      ) +
      geom_segment(
        data = data %>% filter(strand == "-"),
        aes(
          x = recalculated_start, xend = recalculated_end, 
          y = as.numeric(strain) - 0.4, yend = as.numeric(strain)
        ),
        arrow = arrow(type = "closed", length = unit(0.1, "cm")),
        color = "black",
        size = 0.3 # Уменьшаем толщину линии
      )
  }
  
  # Преобразуем ggplot в plotly
  p_plotly <- ggplotly(p, width = 1000)
  
  return(p_plotly)
}
