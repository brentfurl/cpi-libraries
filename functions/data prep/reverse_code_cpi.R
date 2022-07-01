
reverse_code_cpi <- function(df, items) {
  
  for (i in 1:length(items)) {
    df <- df %>% mutate(across(contains(items[i]),
                               ~dplyr::recode(., "Strongly Disagree" = "Strongly Agree",
                                              "Disagree" = "Agree",
                                              "Neither Agree nor Disagree" = "Neither Agree nor Disagree",
                                              "Neither Agree Nor Disagree" = "Neither Agree Nor Disagree",
                                              "Agree" = "Disagree",
                                              "Strongly Agree" = "Strongly Disagree")
    ))
  }

return(df)
}



