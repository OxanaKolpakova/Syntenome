get_centered_df_by_product <- function(df, product_name) {
  # Фильтрация по заданному продукту для каждого штамма
  product_df <- df %>% filter(product == product_name)
  
  # Проверка наличия продукта в датафрейме
  if (nrow(product_df) == 0) {
    message("Product not found: ", product_name)
    return(tibble())
  }
  
  # Создание пустого датафрейма для хранения центрированных данных
  centered_df <- tibble()
  
  # Центрирование координат для каждого штамма и гена отдельно
  strains <- unique(product_df$strain)
  for (selected_strain in strains) {
    strain_product_df <- product_df %>% filter(strain == selected_strain)
    
    # Обход всех генов с указанным продуктом в текущем штамме
    for (i in 1:nrow(strain_product_df)) {
      gene_df <- strain_product_df[i,]
      
      # Вычисление центра координат recalculated_start и recalculated_end для текущего гена
      center <- mean(c(gene_df$recalculated_start, gene_df$recalculated_end))
      
      # Центрирование координат для текущего штамма
      strain_centered_df <- df %>%
        filter(strain == selected_strain) %>%
        mutate(
          recalculated_start = recalculated_start - center,
          recalculated_end = recalculated_end - center,
          strain = ifelse(i > 1, paste0(strain, "_", i), strain)
        )
      
      # Добавление центрированных данных в общий датафрейм
      centered_df <- bind_rows(centered_df, strain_centered_df)
    }
  }
  
  return(centered_df)
}
