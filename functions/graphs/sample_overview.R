sample_overview <- function(df, demo_vars1 = demo_vars){

line_x <- c(.07, .93)
for (d in 1:length(demo_vars1)) {
  pnum <- pnum + 1
  #x <- as.symbol(eval(demo_vars1[d]))
  #d <- 2
  # colnames(df)[5:9] <- c("FOOTBALL OPERATIONS - ROLE", "FOOTBALL OPERATIONS - TEAM","BUSINESS OPERATIONS - ROLE",
  #                              "BUSINESS OPERATIONS - LEVEL", "BUSINESS OPERATIONS - DEPARTMENT") 
  # d=4
  count1 <- df %>%
    group_by(get(demo_vars1[d]), .drop = FALSE) %>%
    summarise(n = n()) %>%
    mutate(`get(demo_vars1[d])` = as.character(str_wrap(`get(demo_vars1[d])`), width=40)) %>%
    filter(!is.na(`get(demo_vars1[d])`)) %>%
    ungroup()
  #mutate(`get(demo_vars1[d])` = factor(`get(demo_vars1[d])`))
  
  levs_narm <- str_wrap(na.exclude(count1$`get(demo_vars1[d])`), width = 40)
  
  count <- count1 %>%
    mutate(`get(demo_vars1[d])` = factor(`get(demo_vars1[d])`, levels = count1$`get(demo_vars1[d])`, ordered = TRUE)) %>% #,levels = levs_narm)) %>%
    # #mutate(`get(demo_vars1[d])` = fct_relevel(`get(demo_vars1[d])`, levs_narm)) %>%
    # mutate(`get(demo_vars1[d])` = str_wrap(`get(demo_vars1[d])`, width = 40)) %>%
    # #mutate(`get(demo_vars1[d])` = factor(`get(demo_vars1[d])`, levels = unique(`get(demo_vars1[d])`), ordered = TRUE)) %>%
    rename(!!eval(demo_vars1[d]) := `get(demo_vars1[d])`) 
  count
  y.axis.text.size <- 17 - max(str_length(levs_narm))/25
  
  
  # x = reorder(eval(b[[1]]), desc(eval(b[[1]]))),
  
  
  
  p <- #ggplot(data = count, aes_(x = reorder(as.name(names(count)[1]), desc(as.name(names(count)[1]))), as.name(names(count)[2]))) + 
    ggplot(data = count, aes(x = reorder(eval(count[[1]]), desc(eval(count[[1]]))), y = n)) +
    geom_col(fill = "#9E2A2F") +
    coord_flip() + 
    theme_classic() +  
    ylab("number of employees") +
    theme(axis.text.y = element_text(size = y.axis.text.size)) +
    sample_overview_theme +
    geom_text(aes(label = paste0(round(count$n/sum(count$n)*100, 1),"%")), hjust = -.3, size = 4.5, family = "Gotham-Medium") +
    scale_y_continuous(expand = expansion(mult = c(.02, .18)))
  
  
  plot <- ggdraw(p) + draw_label(paste0(eval(demo_vars1[d]), ": Count"), fontfamily = "Gotham-Bold", size = 38, x = line_x[1], y = title_y, hjust = 0) +
    draw_line(
      x = line_x,
      y = line_y,
      colour = ("#9E2A2F"), size = 2.2) +
    
    draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
    draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]])
  
  ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), "_",demo_vars1[d], " - COUNT",".pdf"), 
         plot = plot, width = 16, height = 9)
  
}
return(pnum)
}