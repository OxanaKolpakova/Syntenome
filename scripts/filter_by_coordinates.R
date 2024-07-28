filter_by_coordinates <- function(combined_gtf, recalculated_start_pos, recalculated_end_pos) {
  filtered_df <- combined_gtf %>%
    filter(recalculated_start >= recalculated_start_pos & recalculated_end <= recalculated_end_pos)
  
  return(filtered_df)
}