create_contig_boundaries <- function(combined_gtf_adjusted) {
  # Получение уникальных контигов и соответствующих штаммов
  contig_boundaries <- combined_gtf_adjusted %>%
    group_by(seqnames, strain) %>%
    summarize(
      comb_start = min(recalculated_start, na.rm = TRUE),
      comb_end = max(recalculated_end, na.rm = TRUE),
      width = comb_end - comb_start + 1,
      .groups = 'drop'
    ) %>%
    mutate(
      type = "contig_boundary",
      strand = ".",
      source = "boundary",
      score = ".",
      phase = "."
    )
  
  return(contig_boundaries)
}
