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
  
  # Настраиваем расположение легенды в зависимости от значения fill_by
  if (fill_by != "gene") {
    p_plotly <- p_plotly %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                                  xanchor = "left",  # use center of legend as anchor
                                                  x = 0.5,
                                                  y=-0.3,
                                                  width = 800, height = 50)) %>% 
      style(legendgroup = NULL)
  }
  
  return(p_plotly)
}