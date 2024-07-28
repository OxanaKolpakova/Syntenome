concatenate_contigs <- function(combined_gtf) {
  # Вычисление ширины каждого контега
  contig_widths <- combined_gtf %>%
    group_by(strain, seqnames) %>%
    summarize(contig_width = max(end) - min(start), .groups = 'drop')
  
  # Присоединение ширины контега к основному датафрейму
  combined_gtf <- combined_gtf %>%
    left_join(contig_widths, by = c("strain", "seqnames"))
  
  # Функция для пересчета позиций
  recalculate_positions <- function(df) {
    recalculated_start <- c()
    recalculated_end <- c()
    current_position <- 0
    
    # Сортировка по seqnames (по убыванию ширины) и start (по возрастанию)
    df <- df %>%
      arrange(desc(contig_width), seqnames, start)
    
    # Пересчет позиций
    for (i in 1:nrow(df)) {
      recalculated_start <- c(recalculated_start, current_position + 1)
      current_position <- current_position + (df$end[i] - df$start[i] + 1)
      recalculated_end <- c(recalculated_end, current_position)
    }
    
    # Добавление новых столбцов в датафрейм
    df <- df %>%
      mutate(recalculated_start = recalculated_start, 
             recalculated_end = recalculated_end)
    
    return(df)
  }
  
  # Применение функции пересчета позиций к каждому штамму
  concatenated_gtf <- combined_gtf %>%
    group_by(strain) %>%
    group_modify(~ recalculate_positions(.x)) %>%
    ungroup() %>%
    select(-contig_width)  # Удаление временного столбца contig_width
  
  return(concatenated_gtf)
}