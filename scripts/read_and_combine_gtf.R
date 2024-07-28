# Функция для чтения и объединения GTF-файлов
read_and_combine_gtf <- function(gtf_folder) {
  # Получить список файлов GTF в папке
  gtf_files <- list.files(path = gtf_folder, pattern = "\\.gtf$", full.names = TRUE)
  
  # Функция для чтения GTF и добавления столбца strain
  read_gtf_with_strain <- function(file_path) {
    # Извлечь имя файла без пути и расширения
    file_name <- basename(file_path)
    strain <- sub("\\.gtf$", "", file_name)  # Убрать ".gtf" из имени файла
    
    # Импортировать GTF-файл
    gtf_data <- import(file_path)
    
    # Добавить столбец strain
    gtf_data <- as_tibble(gtf_data) %>% 
      mutate(strain = strain)
    
    return(gtf_data)
  }
  
  # Чтение всех GTF файлов и объединение в один датафрейм
  combined_gtf <- gtf_files %>%
    map_df(read_gtf_with_strain)
  
  combined_gtf <- combined_gtf %>% filter(type == "CDS")
  
  # Add gene_product
  combined_gtf <- combined_gtf %>% 
    mutate(gene_product = paste(gene, product, sep = "|"))
  
  return(combined_gtf)
}