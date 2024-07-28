plot_preview <- function(data) {
  # Создаем ggplot
  p <- ggplot(data) +
    aes(xmin = comb_start, xmax = comb_end, ymin = as.numeric(strain) - 0.4, ymax = as.numeric(strain) + 0.4, fill = seqnames) +
    geom_rect(
      color = "black",# использовать только при малом масштабе
      ) +
    scale_y_continuous(breaks = seq_along(levels(data$strain)), labels = levels(data$strain)) +
    theme_minimal() +
    labs(x = "", y = "Strain", fill = "Seqnames")
  
  # Преобразуем ggplot в plotly
  p_plotly <- ggplotly(p, width=1000)
  return(p_plotly)
}