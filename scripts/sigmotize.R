# Определяем функцию сигмоида
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# Определяем функцию для применения сигмоида к датафрейму
sigmotize <- function(df) {
  df %>%
    mutate(
      start = sigmoid(start),
      end = sigmoid(end)
    )
}