get_invert_strains_by_product <- function(df, product_name) {
  product_df <- df %>% filter(product == product_name)
  
  if (nrow(product_df) == 0) {
    message("Product not found: ", product_name)
    return(c())
  }
  
  invert_strains <- product_df %>%
    filter(strand == "-") %>%
    pull(strain) %>%
    unique()
  
  return(invert_strains)
}