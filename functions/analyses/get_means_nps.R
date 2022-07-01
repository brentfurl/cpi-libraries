
get_means_nps <- function(df, demo_vars) {

demo_vars <- c("ALL", demo_vars)
  
means_nps_loop <- list()
for (d in 1:length(demo_vars))  {
  
  #d<-1
  demo_var <- demo_vars[[d]]
nps <- df %>% count(get(demo_var), nps_group) %>%
  filter(nps_group != "NA") %>% 
  #filter(`get(demo_var)` != "NA") %>%
  pivot_wider(names_from = nps_group, values_from = n) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rowwise() %>%
  mutate(nps_calc = ((Promoter/sum(c(Detractor, Passive, Promoter), na.rm = TRUE))-(Detractor/sum(c(Detractor, Passive, Promoter), na.rm = TRUE))) *100) %>%
  rename(level = colnames(.)[1]) %>%
  #rename(level_loc = colnames(.)[2]) %>%
  mutate(level = as.character(level)) %>%
  filter(!is.na(level)) %>%
  mutate(level = factor(level)) %>%
  add_column(demo = eval(demo_var), .before = 1) %>%
  select(1:2, nps_calc, everything()) 
  
means <- df %>% group_by(get(demo_var)) %>%
  summarise(n = n(),CPI_index = round(mean(CPI_index, na.rm = TRUE),0),
            Direction = round(mean(Direction, na.rm = TRUE),0), Operations = round(mean(Operations, na.rm=TRUE),0),
            People = round(mean(People, na.rm = TRUE),0), Engagement = round(mean(Engagement, na.rm=TRUE),0),
            Vision = round(mean(Vision, na.rm = TRUE),0), Strategy = round(mean(Strategy, na.rm=TRUE),0),
            Leadership = round(mean(Leadership, na.rm = TRUE),0), Adaptability = round(mean(Adaptability, na.rm=TRUE),0),
            Performance = round(mean(Performance, na.rm = TRUE),0), Systems = round(mean(Systems, na.rm=TRUE),0),
            Teamwork = round(mean(Teamwork, na.rm = TRUE),0), Talent = round(mean(Talent, na.rm=TRUE),0),
            Development = round(mean(Development, na.rm = TRUE),0), Fit = round(mean(Fit, na.rm=TRUE),0),
            Customer = round(mean(Customer, na.rm = TRUE),0), Climate = round(mean(Climate, na.rm=TRUE),0),
            Positivity = round(mean(Positivity, na.rm = TRUE),0)) %>%#Satisfaction = round(mean(`extra_sat_satisfaction_1_Taking everything into consideration, I am satisfied with my job as a whole.`, na.rm=TRUE),0),
            #Effort = round(mean(`extra_eff_effort_2_I am willing to put in a great deal of effort beyond what is normally expected in order to help the organization.`, na.rm=TRUE),0),
            #nps = round(mean(nps, na.rm=TRUE),0)) %>%
  ungroup() %>%
  mutate(`get(demo_var)` = as.character(`get(demo_var)`)) %>%
  filter(!is.na(`get(demo_var)`)) %>%
  mutate(`get(demo_var)` = factor(`get(demo_var)`)) %>%
  rename(level = colnames(.)[1]) 

nps_means <- full_join(nps, means)
means_nps_loop[[d]] <- nps_means
}
nps_means_demos <- do.call(rbind, means_nps_loop)
saveRDS(nps_means_demos, paste0(file.analyses, "means_nps.rds")) 
write_csv(nps_means_demos, paste0(file.analyses, "means_nps.csv")) 

return(nps_means_demos)

}
 
