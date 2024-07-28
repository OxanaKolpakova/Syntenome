plot_gtf <- function(data, fill_by="product") {
  # Проверяем, существует ли указанный столбец в данных
  if (!fill_by %in% colnames(data)) {
    stop(paste("Столбец", fill_by, "не найден в данных"))
  }
  
  # Создаем ggplot
  p <- ggplot(data) +
    aes(xmin = recalculated_start, xmax = recalculated_end, ymin = as.numeric(strain) - 0.4, ymax = as.numeric(strain) + 0.4, fill = .data[[fill_by]]) +
    geom_rect(
      color = "black"  # использовать только при малом масштабе
    ) +
    scale_y_continuous(breaks = seq_along(levels(data$strain)), labels = levels(data$strain)) +
    theme_minimal() +
    labs(x = "Relative Coordinates", y = "Strain", fill = fill_by)
  
  # Преобразуем ggplot в plotly
  p_plotly <- ggplotly(p, width = 1000)
  return(p_plotly)
}