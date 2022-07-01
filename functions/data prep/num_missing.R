num_missing <- function(df) {
  
  sum_missing_each_col <- df  %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.)))) 

  return(tibble(columns = colnames(df), missing = map_int(unlist(as.list(sum_missing_each_col[1,])), ~.x)))
}
  
# y <- num_missing(df_recoded)
# write_csv(y, paste0(file.data, "num_missing.csv"))
