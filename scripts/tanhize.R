tanhize <- function(df) {
  # Проверка, что в датафрейме есть нужные колонки
  if(!all(c("recalculated_start", "recalculated_end") %in% names(df))) {
    stop("Датафрейм должен содержать колонки 'recalculated_start' и 'recalculated_end'")
  }
  
  # Применение гиперболического тангенса к колонкам start и end
  df %>%
    mutate(
      recalculated_start = tanh(recalculated_start / 10000) ,
      recalculated_end = tanh(recalculated_end / 10000)
    )
}