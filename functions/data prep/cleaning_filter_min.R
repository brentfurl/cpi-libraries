

cleaning_filter_min <- function(df, missing) {
  
  df <- df %>%
  filter(
       df$CPI_missing < missing)
      

  return(df)
}


