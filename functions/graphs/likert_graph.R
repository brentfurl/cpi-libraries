#likert_var <- "extra2_likert_pressureSupport_1_How would you rate the support available to help you with the pressure?"

likert_graph <- function(pnum, df, likert_var) {
  pnum <- pnum + 1
  if (grepl("JobSat", likert_var) == TRUE) {
    df_plot <- df %>%
      mutate(!!likert_var := factor(get(likert_var), levels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"), ordered = TRUE)) %>% 
      group_by(get(likert_var), .drop = FALSE) %>%
      count() %>% 
      #summarise(x = count()) %>% 
      drop_na() %>% 
      rename(rating = `get(likert_var)`) 
  } else {
  df_plot <- df %>%
    mutate(!!likert_var := factor(get(likert_var), levels = c("Very Weak", "Weak", "Neither Strong nor Weak", "Strong", "Very Strong"), ordered = TRUE)) %>% 
    group_by(get(likert_var), .drop = FALSE) %>%
    count() %>% 
    #summarise(x = count()) %>% 
    drop_na() %>% 
    rename(rating = `get(likert_var)`) 
}

p <- ggplot(data = df_plot, aes(x = rating, y = n)) + 
  geom_col(position=position_dodge(width = -.8), fill = "#9E2A2F")  + 
  coord_flip() +
  #coord_flip(ylim = c(0, 10.5)) + 
  #scale_y_continuous(breaks = c(0, 2,4,6,8,10)) +
  scale_x_discrete(limits = rev(unique(df_plot$rating))) +
  geom_text(aes(label = paste0(round(df_plot$n/sum(df_plot$n)*100,1), "%")), hjust = -.3, size = 4.5, family = "Gotham-Medium") +
  #geom_text(aes(label = value), data = df_plot %>% na_if(.,0), vjust = .5, hjust = -0.4, size = 4, family = "Gotham-Medium", position = position_dodge(width= -.8)) + 
  theme_cowplot() + 
  #guides(fill =  guide_legend(ncol = var_groups[[g]][[4]])) +
  #theme(legend.position = "bottom", legend.title = element_blank()) +
  #scale_fill_manual(overall_value_CEDs = demo_colors[[1]][[2]]) +
  sample_overview_theme +
  #demo_dimension_theme +
  ylab("Number of responses") +
  scale_y_continuous(expand = expansion(mult = c(.02, .18)))

plot <- ggdraw(p) + draw_label(str_wrap(toupper(sub(".*_", "", eval(likert_var))), width=45), fontfamily = "Gotham-Bold", size = 25, x = line_x[1], y = title_y, hjust = 0) +
  draw_line(
    x = line_x,
    y = line_y,
    colour = ("#9E2A2F"), size = line_size) +
  draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
  draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]])

ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), eval(likert_var),".pdf"),
       plot = plot, width = 16, height = 9)

return(pnum)
#-------------------------------------------------------------------
}