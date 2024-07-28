get_centered_df_by_gene <- function(df, gene_name, reverse_strand = FALSE) {
  # Фильтрация по заданному гену
  gene_df <- df %>% filter(gene == gene_name)
  
  # Проверка наличия гена в датафрейме
  if (nrow(gene_df) == 0) {
    message("Gene not found: ", gene_name)
    return(tibble())
  }
  
  # Создание пустого датафрейма для хранения центрированных данных
  centered_df <- tibble()
  
  # Центрирование координат для каждого штамма и гена отдельно
  strains <- unique(gene_df$strain)
  for (selected_strain in strains) {
    strain_gene_df <- gene_df %>% filter(strain == selected_strain)
    
    # Обход всех генов с одинаковым названием в текущем штамме
    for (i in 1:nrow(strain_gene_df)) {
      gene_instance_df <- strain_gene_df[i,]
      
      # Вычисление центра координат recalculated_start и recalculated_end для текущего гена
      center <- mean(c(gene_instance_df$recalculated_start, gene_instance_df$recalculated_end))
      
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
