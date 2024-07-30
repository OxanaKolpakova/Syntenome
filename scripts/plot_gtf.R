plot_gtf <- function(data, fill_by = "product", show_legend = TRUE, invert_strains = NULL) {
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
      color = "black"  # использовать только при малом масштабе
    ) +
    scale_y_continuous(breaks = seq_along(levels(data$strain)), labels = levels(data$strain)) +
    theme_minimal() +
    labs(x = "Relative Coordinates", y = "Strain", fill = fill_by) +
    theme(legend.position = if (show_legend) "right" else "none")
  
  # Преобразуем ggplot в plotly
  p_plotly <- ggplotly(p, width = 1000)
  
  return(p_plotly)
}
