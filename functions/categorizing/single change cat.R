single_change_cat <- function(df_Change){

  keywords <- read_sheet("https://docs.google.com/spreadsheets/d/1_-p6FvD-vFjHdGBMdOvimH8Tmw4x1FCkbQ_EL9koAuQ/edit#gid=0")
  
  keywords_tib <- keywords %>% 
    pivot_longer(DIRECTION:CLIMATE, names_to = "dimension") %>% 
    rename(word = "value") %>% 
    filter(!is.na(word))  
  
df_antijoin <- tibble(word = c("single", "change", company, tolower(company)))

df <- df_Change %>%
  rename(responses =1) %>% 
  rownames_to_column() 
#------------------------------------------------------------------------
df_var_tok <- df %>% 
  unnest_tokens(word, responses) %>% 
  anti_join(stop_words) %>% 
  anti_join(df_antijoin)

df_var_freq <- df_var_tok %>% 
  count(word, sort = TRUE) %>% 
  anti_join(keywords_tib) %>% 
  slice_head(prop = .05) 

matches  <- df_var_tok %>% 
  left_join(keywords_tib, by = "word") %>% 
  mutate(keyword = ifelse (!is.na(dimension), word, ifelse(is.na(dimension), NA, 0))) 

matched <- matches %>% 
  filter(!is.na(keyword)) %>% 
  group_by(rowname) %>% 
  summarise(dimensions = paste(unique(dimension), collapse = ","), keywords = paste(unique(keyword), collapse = ","), num_keywords = n_distinct(keyword)) 

text_cat_keywords <- left_join(df, matched) %>%
  arrange(desc(num_keywords))

df_matched_freq <- df_var_tok %>% 
  full_join(df_var_freq)


matched_freq <- df_matched_freq %>% 
  filter(!is.na(n)) %>% 
  group_by(rowname) %>% 
  summarise(frequencies = sum(n), freq_words = paste(unique(word), collapse = ","), num_frequent_words = n_distinct(word)) 

text_cat_freq <- left_join(df, matched_freq) %>%
  arrange(desc(num_frequent_words))

text_freq_plus_keywords <- text_cat_keywords %>% 
  select(-(responses)) %>% 
  left_join(text_cat_freq, by = "rowname") %>% 
  select(rowname, responses, dimensions, keywords, num_keywords, freq_words, frequencies, num_frequent_words)

write_csv(text_freq_plus_keywords, paste0("~/Dropbox/Clients/", company, "/data/oeqs/categorized/df_Change.csv"))
#write_csv(text_freq_plus_keywords, paste0("~/Dropbox/DCI/Clients/", company, "/open ended categorized with app/df_Change.csv"))
}