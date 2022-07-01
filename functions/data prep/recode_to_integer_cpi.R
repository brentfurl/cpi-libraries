recode_to_integer_cpi <- function(df, prefixes) {
  
  for (p in 1:length(prefixes)) {
  
    df <- df %>% mutate(across(contains(prefixes[p]),
                               ~dplyr::recode(., 
                                              "Strongly Disagree" = "0", 
                                              "Disagree" = "25", 
                                              "Neither Agree nor Disagree" = "50", 
                                              "Neither Agree Nor Disagree" = "50", 
                                              "Agree" = "75", 
                                              "Strongly Agree" = "100"))) %>% 
                mutate(across(contains(prefixes[p]), as.integer))
  
  }
  
 
return(df)
}