cat_oeqs <- function(df_oeq) {
  
  keywords <- read_sheet("https://docs.google.com/spreadsheets/d/1_-p6FvD-vFjHdGBMdOvimH8Tmw4x1FCkbQ_EL9koAuQ/edit#gid=0")
  
  keywords_tib <- keywords %>% 
    pivot_longer(DIRECTION:CLIMATE, names_to = "dimension") %>% 
    rename(word = "value") %>% 
    filter(!is.na(word))
  
  #df_oeq <- readRDS(paste0("~/Dropbox/Clients/", company, "/data/df_oeq.rds"))
  
  colname_tib <- tibble(oeqs = colnames(df_oeq)[colnames(df_oeq) != "ResponseId"]) %>% 
    mutate(predash = sub("\\_.*", "", oeqs)) 
  
  multi_answers <- colname_tib %>% 
    count(predash) %>% 
    filter(n > 1) %>% 
    .[["predash"]]
  
  single_answers <- colname_tib %>% 
    count(predash) %>% 
    filter(n == 1) %>% 
    left_join(.,colname_tib) %>% 
    .[["oeqs"]]
  
  if (length(multi_answers) >0) {  
    
    dfs_mult_names <- c()
    for (m in 1:length(multi_answers)) {
      # m=1
      assign(paste0("df_", multi_answers[m]), df_oeq %>%  
               select(starts_with(multi_answers[m])) %>% 
               pivot_longer(., cols = starts_with(multi_answers[m]), names_to = "measure", values_to = multi_answers[m]) %>% 
               select(multi_answers[m]) %>% 
               filter(!is.na(get(multi_answers[m]))))
      
      saveRDS(get(paste0("df_", multi_answers[m])), paste0("~/Dropbox/Clients/", company, "/data/oeqs/datasets/", paste0("df_", multi_answers[m]), ".rds"))
      
      dfs_mult_names[m] <- paste0("df_", multi_answers[m])
      
    }
  }
  
  dfs_single_names <- c()
  for (m in 1:length(single_answers)) {
    
    assign(paste0("df_", single_answers[m]), df_oeq %>%  
             select(starts_with(single_answers[m])) %>% 
             filter(!is.na(get(single_answers[m]))))
    
    saveRDS(get(paste0("df_", single_answers[m])), paste0("~/Dropbox/Clients/", company, "/data/oeqs/datasets/", paste0("df_", single_answers[m]), ".rds"))
    
    dfs_single_names[m] <- paste0("df_", single_answers[m])
    
  }
  
  df_allnames_oeqs <- c(dfs_single_names[dfs_single_names != "df_Change"], dfs_mult_names)
  # load them if not already loaded
  
  for (d in 1:length(df_allnames_oeqs)) {
    # d=3
    if (tolower(colnames(get(df_allnames_oeqs[d])[1])) == "best day") {
      df_antijoin <- tibble(word = c("Best", "best", "day", "days", company, tolower(company)))
    } else {
      df_antijoin = tibble(word = "")
    }
    #-----------------------------------------------------------------------    
    df <- get(df_allnames_oeqs[d]) %>%
      rename(responses =1) %>% 
      rownames_to_column() 
    #------------------------------------------------------------------------
    df_var_tok <- df %>% 
      unnest_tokens(word, responses) %>% 
      anti_join(stop_words) %>% 
      anti_join(df_antijoin)
    
    df_var_freq <- df_var_tok %>% 
      count(word, sort = TRUE) %>% 
      rename(freq = "n") %>% 
      anti_join(keywords_tib) %>% 
      slice_head(prop = .05) %>% 
      mutate(rank = 1:n()) %>% 
      select(word, rank, freq)
    
    # text_cat_keywords <- left_join(df, matched) %>%
    #   arrange(desc(num_keywords))
    
    df_matched_freq <- df_var_tok %>% 
      full_join(df_var_freq) 
    
    
    matched_freq <- df_matched_freq %>% 
      filter(!is.na(freq)) %>% 
      group_by(rowname) %>% 
      summarise(top_rank = min(rank), freq_words = paste(unique(word), collapse = ","), max_freq = max(freq), num_frequent_words = n_distinct(word)) 
    
    text_cat_freq <- left_join(df, matched_freq) %>%
      arrange(desc(num_frequent_words), top_rank)
    
    
    wb <- createWorkbook()
    addWorksheet(wb, "responses")
    writeData(wb, "responses", text_cat_freq)
    addWorksheet(wb, "frequencies")
    writeData(wb, "frequencies", df_var_freq)
    
    saveWorkbook(wb, file = paste0("~/Dropbox/Clients/", company, "/data/oeqs/categorized/", df_allnames_oeqs[d], ".xlsx"), overwrite = TRUE)
    saveWorkbook(wb, file = paste0("~/Dropbox/DCI/Clients/", company, "/open ended categorized with app/", df_allnames_oeqs[d], ".xlsx"), overwrite = TRUE)
    # write_csv(text_freq_plus_keywords, paste0("~/Dropbox/DCI/Clients/", company, "/open ended categorized with app/", df_allnames_oeqs[d], ".csv"))
  }
  
}