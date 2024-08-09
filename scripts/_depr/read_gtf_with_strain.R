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