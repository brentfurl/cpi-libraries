
appendix_hor <- function(pnum, df, df_long, demo_vars_hor) {

logo_scale_coords <- list(c(.11, .43, -.417), c(.173, .45, -.40), c(.11, .43, -.417), c(.173, .45, -.40), c(.15, .43, -0.417))
# EDIT HERE:
quadrants <- list(list("Direction",c("Vision", "VISION", "vision"), c("Strategy", "STRATEGY", "strategy"), c("Leadership", "LEADERSHIP", "leadership")), 
                  list("Operations",c("Adaptability", "ADAPTABILITY"), c("Performance","PERFORMANCE","performance"), c("Systems", "SYSTEMS", "systems")), 
                  list("People",c("Teamwork","TEAMWORK"), c("Talent", "TALENT"), c("Development", "DEVELOPMENT", "development")), 
                  list("Engagement",c("Fit", "FIT"), c("Customer", "CUSTOMER"), c("Climate", "CLIMATE")),
                  list("PositivitySatEff", c("Positivity", "POSITIVITY"), c("Satisfaction + Effort", "SATISFACTION + EFFORT")))

#all_items_cpi <- unique(df_long$item)[1:67]
# all_items <- c(all_items_cpi, "People in the district express genuine gratitude toward one another.", "I practice mindfulness by staying in the present moment while doing my work.",
#                "People I work with generally feel optimistic about the district's future.", "People that I work with care about my happiness and well-being.", 
#                "I feel inspired every day to do my best work.", "Taking everything into consideration, I am satisfied with my job as a whole.",
#                "I am willing to put in a great deal of effort beyond what is normally expected in order to help the district.")
  #########################################################################

#for (d in 1:length(demo_vars_hor)) {
pnum = pnum + 1
  p <- ggplot() + theme(plot.background = element_rect(fill = "#56565A"),
                        panel.background = element_rect(fill = "#56565A"))
  
  plot <- ggdraw(p) +
    draw_label(str_wrap(toupper(sub(" -.*", "",   sub(".*_", "", eval(demo_vars_hor)))), width = 35), fontfamily = "Gotham Black", size = 53, x = .2, y = .54, hjust = 0, color = "white", alpha = .9) +
    #draw_label(paste0("n = ", nrow(df)), fontfamily = "Gotham Black", size = 50, x = .82, y = .08, hjust = 0, color = "white") +
    draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
    draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
  
  ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), "_app title_",demo_vars_hor, ".pdf"), 
         plot = plot, width = 16, height = 9) 
#------------------------------- GET DATASETS ------------------------------------------------  
  
  
 
  df_level_filters <- df %>%
    group_by(get(demo_vars_hor)) %>%
    filter(n() > 4) %>%
    #filter(get(demo_vars_hor) != <NA>)
    ungroup() %>%
    droplevels()
  
#levels <- str_wrap(levels(df[[demo_vars_hor]])[levels(df[[demo_vars_hor]]) %in% levels(df_level_filters[[demo_vars_hor]])], width = 65)
 levels <-levels(df[[demo_vars_hor]])[levels(df[[demo_vars_hor]]) %in% levels(df_level_filters[[demo_vars_hor]])]
  
  df_level_filters <- df_level_filters %>%
    mutate(!!demo_vars_hor := as.character(get(demo_vars_hor))) %>%
    filter(!is.na(get(demo_vars_hor))) %>%
    mutate(!!demo_vars_hor :=  factor(get(demo_vars_hor))) %>%
    #mutate(!!demo_vars_hor :=  fct_relevel(get(demo_vars_hor), na.omit(levels)))
    mutate(!!demo_vars_hor :=  fct_relevel(get(demo_vars_hor), na.omit(levels)))

  # if (demo_vars_hor == "GENERATION") {
  #   df_level_filters <- df_level_filters %>%
  #     mutate(GENERATION = factor(GENERATION, 
  #                                levels = c("Baby Boomers (born between 1946 - 1965)", "Generation X (born between 1966 - 1980)", "Generation Y (born between 1981 - 2000)")),
  #            ordered = TRUE)
  # }                                            
  #                                                  
  
  
  if (length(levels) > 4) {numcol_legend = 2
  } else {numcol_legend = 1}
  
  colors <- demo_colors[[1]][[2]][1:length(levels(df_level_filters[[eval(demo_vars_hor)]]))]
  
  df_long_level_filters <- df_long %>% 
    group_by(get(demo_vars_hor)) %>% 
    filter(n() > 4*length(all_cpi_vars)) %>%
    droplevels() %>%
    ungroup() %>%
    mutate(!!demo_vars_hor := as.character(get(demo_vars_hor))) %>%
    filter(!is.na(get(demo_vars_hor))) %>%
    mutate(!!demo_vars_hor :=  factor(get(demo_vars_hor))) %>%
    mutate(!!demo_vars_hor :=  fct_relevel(get(demo_vars_hor), na.omit(levels)))
  
  # if (demo_vars_hor == "GENERATION") {
  #   df_long_level_filters <- df_long_level_filters %>%
  #     mutate(GENERATION = factor(GENERATION, 
  #                                levels = c("Baby Boomers (born between 1946 - 1965)", "Generation X (born between 1966 - 1980)", "Generation Y (born between 1981 - 2000)")),
  #            ordered = TRUE)
  # }                                        
  
#---------------------------------------------- QUADRANT PLOTS ---------------------------------------  
  
  for (q in 1:4) {
 pnum <- pnum + 1
 
 df_quad_plot_pre <- df_long_level_filters %>%
   filter(quadrant == eval(quadrants[[q]][[1]])) %>%
   group_by(get(demo_vars_hor), dimension) %>%
   summarise(score = mean(value, na.rm = TRUE)) %>% 
   ungroup()
 
 levels_dim <- toupper(unique(df_quad_plot_pre$dimension))
 
 df_quad_plot <- df_quad_plot_pre   %>%
   mutate(dimension = as.character(toupper(dimension))) %>% 
   mutate(dimension = factor(dimension, levels = levels_dim, ordered = TRUE))
    
    
   
    
    #if (q == 2){
      p <- ggplot(data = df_quad_plot, aes(df_quad_plot[[1]], score, fill = df_quad_plot[[1]])) + 
        geom_col() +  
        theme_cowplot() + 
        coord_cartesian(ylim = c(0, 115)) + 
        scale_y_continuous(breaks = c(0, 50, 100)) +
        facet_grid(dimension ~ .  , switch = "y") +
        #scale_x_discrete(breaks = unique(df_quad_plot[[1]])) +
        scale_fill_manual(values = c(colors), breaks = unique(df_quad_plot[[1]])) +
        geom_text(aes(label = round(score, 0)), vjust = -.5, size = 4.2, family = "Gotham-Medium") +
        dem_quad_theme +
        theme(legend.margin = margin(10, 0, -10, 20)) +
        guides(fill = guide_legend(ncol = numcol_legend)) 
    
    
    
    p2 <- grid.arrange(p, nrow=1)
    plot <- ggdraw(p2) + draw_label(paste0(eval(demo_vars_hor), ": ", eval(quadrants[q][[1]][[1]])), fontfamily = "Gotham-Bold", size = title_size, x = line_x[[1]],  y = title_y, hjust = 0) +
      draw_line(
        x = line_x,
        y = line_y,
        colour = ("#9E2A2F"), size = 2.2) +
      draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
      draw_image(paste0(file.images, quadrants[[q]][[1]], ".png"), scale = logo_scale_coords[[q]][[1]], x = logo_scale_coords[[q]][[2]], y = logo_scale_coords[[q]][[3]]) +
      draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]])
      # 
    ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), "_", eval(demo_vars_hor), "_", eval(quadrants[q][[1]][[1]]),".pdf"), 
           plot = plot, width = 16, height = 9)
    
   
  }
   #pnum <- 127
  for (q in 1:4) { 
    
   
    
    for (i in 2:4) {
      ### run the dim level plot  
      
             # q <- 1
             # d <- 5
             # i <- 2
      
      level_nums <- 1:5
      geom_text_size <- 3.2
      
      if (i == 2) {
        item_range <- eval(q * 15 - 14):eval(q * 15 - 10)
        item_labels <- all_items[item_range]
        #item_labels <- all_items[eval(q * 15 - 14):eval(q * 15 - 10)]
        cpi_vars <- all_cpi_vars[eval(q * 15 - 14):eval(q * 15 - 10)]}
      if (i == 3) {
        item_range <- eval(q * 15 - 9):eval(q * 15 - 5)
        item_labels <- all_items[item_range]
        #items_labels <- all_items[eval(q * 15 - 9):eval(q * 15 - 5)]
        cpi_vars <- all_cpi_vars[eval(q * 15 - 9):eval(q * 15 - 5)]}
      if (i == 4) {
        item_range <- eval(q * 15 - 4):eval(q * 15)
        item_labels <- all_items[item_range]
        #items_labels <- all_items[eval(q * 15 - 4):eval(q * 15)]
        cpi_vars <- all_cpi_vars[eval(q * 15 - 4):eval(q * 15)]}
      
      #dimension <- quadrants[[q]][[i]][1]
      
  ########################################## PLOT BEGIN ###########################
      
      pnum <- pnum + 1
    item_labels_wrapped <- str_wrap(item_labels, width = 47) # items_wrapped
      
      
      ## do this next and try to get a Adaptability values one to work
      long_dim  <- df_level_filters %>% gather(dim_items, Items, cpi_vars)  # long_dim, dim_items, Items, 
      #long_dim <- long_dim %>% select(ResponseId, Role:Gender, All, dim_items, Items)
      long_dim$dim_items  <- parse_number(long_dim$dim_items)
      long_dim <- long_dim %>% mutate(dim_items = factor(dim_items, levels = level_nums, labels = item_labels_wrapped))
        
      
      
      # do this 12 times for each dimension and then repeat for Tenure Generation.  Make code consistent enough to just copy and paste Tenure and Generation.
      long_dim <- long_dim %>%
        group_by(get(demo_vars_hor),dim_items)
      dim_Means <- summarise(long_dim, mean(Items, na.rm = TRUE)) #%>%
        #mutate(`get(demo_vars_hor)` = fct_relevel(`get(demo_vars_hor)`, str_wrap(levels(long_dim[[demo_vars_hor]]), width = 65))) 
        #mutate(`get(demo_vars_hor)` = factor(`get(demo_vars_hor)`, ordered= TRUE))
      colnames(dim_Means) <- c(eval(demo_vars_hor), "Items", "Means")
      dim_Means <- dim_Means %>%
        #mutate(!!eval(demo_vars_hor := fct_relevel(get(demo_vars_hor), levels = na.omit(levels)))) %>%
        filter(get(demo_vars_hor) != "NA" & Means != "NaN") 
    
      #### DIMENSION DEMO PLOTS Q 1-4
        
      # p <- ggplot(data = dim_Means, aes(x = reorder(eval(dim_Means[[1]]), desc(eval(dim_Means[[1]]))), y = Means, fill = eval(dim_Means[[1]]))) +     #, fill = eval(demo_vars_hor), alpha=5)) + 
      #   geom_col()+ coord_flip(ylim = c(0, 100)) + 
      #   guides(alpha=FALSE) + 
      #   theme_cowplot()   + 
      #   facet_grid(Items ~ ., switch = "y") + 
      #   geom_text(aes(label = round(Means, 0)), hjust = -.3, size = 3, family = "Gotham-Medium") + 
      #   scale_fill_manual(values = demo_colors[[1]][[2]]) +
      #   demo_dimension_theme +
      #   guides(fill =  guide_legend(ncol = numcol_legend))
      
      #p <-  ggplot(data = dim_Means, aes(x = Items, Means, fill = str_wrap(eval(dim_Means[[1]]), width=65))) +
      # p <-  ggplot(data = dim_Means, aes(x = Items, Means, fill = dim_Means[[1]])) +
      #   geom_bar(position=position_dodge(width = .8), stat="identity") +
      #   guides(fill =  guide_legend(ncol = numcol_legend)) +
      #   theme_cowplot() + 
      #   theme(legend.position = "bottom", legend.title = element_blank())+
      #   geom_text(aes(label = round(Means, 0)), vjust = -.5, size = geom_text_size, family = "Gotham-Medium", position = position_dodge(width=.8)) + 
      #   coord_cartesian(ylim = c(0, 100)) + 
      #   scale_y_continuous(breaks = c(0, 50, 100)) +
      #   theme(plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
      #         axis.title.x = element_blank(),
      #         axis.title.y = element_blank(),
      #         axis.text.y = element_text(family = "Gotham-Bold", face = "bold", hjust = 1),
      #         axis.text.x = element_text(size = 10, family = "Gotham-Bold", colour = "#3d3d3d", hjust = 0.5),
      #         
      #         legend.text = element_text(size = legend_text,family = "Gotham-Medium"),
      #         axis.line.x = element_blank(), 
      #         axis.ticks.x = element_blank(), 
      #         legend.margin = margin(10, 0, -10, 40)) +
      #   scale_fill_manual(values = c("#9E2A2F", "#75787B", "#333333", "#E6E0D3", "#4A6B7D", "#556B59", 
      #                                "#B17E4A", "#C65C3D", "#62486D", "#9E2A2F", "#75787B", "#333333", "#E6E0D3", "#4A6B7D"))
      
      p <- ggplot(data = dim_Means, aes(x = reorder(eval(dim_Means[[1]]), desc(eval(dim_Means[[1]]))), y = Means, fill = eval(dim_Means[[1]]))) +     #, fill = eval(demo_vars_hor), alpha=5)) + 
        geom_col()+ coord_flip(ylim = c(0, 100)) + 
        guides(alpha=FALSE) + 
        theme_cowplot()   + 
        facet_grid(Items ~ ., switch = "y") + 
        geom_text(aes(label = round(Means, 0)), hjust = -.3, size = 3, family = "Gotham-Medium") + 
        scale_fill_manual(values = demo_colors[[1]][[2]]) +
        demo_dimension_theme +
        guides(fill =  guide_legend(ncol = numcol_legend))
        
      plot <- ggdraw(p) + 
        draw_label(paste0(eval(demo_vars_hor), ": ", eval(quadrants[[q]][[i]][1])), 
            fontfamily = "Gotham-Bold", size = title_size, x = line_x[[1]],  y = .92, hjust = 0) +
        draw_line(
          x = line_x,
          y = line_y,
          colour = ("#9E2A2F"), size = 2.2) +
            draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
            draw_image(paste0(file.images, quadrants[[q]][[1]], ".png"), scale = logo_scale_coords[[q]][[1]], x = logo_scale_coords[[q]][[2]], y = logo_scale_coords[[q]][[3]]) +
            draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]])

      if (grepl("*", paste(item_labels, collapse = ''), fixed = TRUE) == TRUE) {
        plot <- plot +
          draw_label("*", fontfamily = "Gotham-Bold", size = 14, x = .23,  y = .02, hjust = 0) +
          draw_label("reverse scored", fontfamily = "Gotham-Medium", size = 10, x = .24,  y = .02, hjust = 0)
          #draw_label("Scores for these questions are reversed so that higher scores represent better outcomes.", fontfamily = "Gotham-Book", size = 10, x = .24,  y = .02, hjust = 0)
      }
      
      
      ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), "_",demo_vars_hor, "_", eval(quadrants[[q]][[i]][1]),".pdf"), 
             plot = plot, width = 16, height = 9)
      
      
    } # i- dimension loop
    }# quadrant loop
     # closes IF q == 1:4
      # makeplot_item_level <- function{CREATE}
      # plot_item_level <- function(makeplot_item_level)....applies current 'items' and 'dimension' name
 for (q in 5) {
   
     for (i in 2:3) 
     {
       pnum <- pnum + 1
             # d<-7
             # q <- 5
             # i <- 3   #hjust for generation sat and effort on this one was manually put to .8 instead of -.2
       # if ((d == 4 & i ==3) | (d == 5 & i ==3) | (d== 6 & i ==3) | (d==7 & i ==3)) {
       #   title_txt_size = 32
       # }else {
       #   title_txt_size = 38
       # }
       
       if (i == 2) {
         item_range <- eval(q * 15 - 14):eval(q * 15 - 10)
         item_labels <- all_items[item_range]
         #item_labels <- all_items[eval(q * 15 - 14):eval(q * 15 - 10)]
         cpi_vars <- all_cpi_vars[eval(q * 15 - 14):eval(q * 15 - 10)]
         level_nums <- 1:5
         geom_text_size <- 3
         y_range <- c(0, 100)
         }
       if (i == 3) {
         item_range <- eval(q * 15 - 9):eval(q * 15 - 8)
         item_labels <- all_items[item_range]
         #items_labels <- all_items[eval(q * 15 - 9):eval(q * 15 - 5)]
         cpi_vars <- all_cpi_vars[eval(q * 15 - 9):eval(q * 15 - 8)]
         level_nums <- 1:2
         geom_text_size <- 3.4
         y_range <- c(0, 102)
         }
       
       
       
       
       #dimension <- quadrants[[q]][[i]][1]
       
       ########################################## PLOT BEGIN ###########################
       
       
       #item_labels = all_items[16:20]  # # this is items from above
       item_labels_wrapped <- str_wrap(item_labels, width = 47) # items_wrapped
       
       
       ## do this next and try to get a Adaptability values one to work
       long_dim  <- df_level_filters %>% gather(dim_items, Items, cpi_vars)  # long_dim, dim_items, Items, 
       #long_dim <- long_dim %>% select(ResponseId, Role:Gender, All, dim_items, Items)
       long_dim$dim_items  <- parse_number(long_dim$dim_items)
       long_dim <- long_dim %>% mutate(dim_items = factor(dim_items, levels = level_nums, labels = item_labels_wrapped))
       
       
       
       # do this 12 times for each dimension and then repeat for Tenure Generation.  Make code consistent enough to just copy and paste Tenure and Generation.
       long_dim <- long_dim %>%
         group_by(get(demo_vars_hor),dim_items)
       dim_Means <- summarise(long_dim, mean(Items, na.rm = TRUE)) %>%
         mutate(`get(demo_vars_hor)` = fct_relevel(`get(demo_vars_hor)`, str_wrap(levels(long_dim[[demo_vars_hor]]), width = 47))) 
       #mutate(`get(demo_vars_hor)` = factor(`get(demo_vars_hor)`, ordered= TRUE))
       colnames(dim_Means) <- c(eval(demo_vars_hor), "Items", "Means")
       dim_Means <- dim_Means %>%
         filter(get(demo_vars_hor) != "NA" & Means != "NaN") 
       
       #### POS/SAT_EFFORT DEMO PLOTS Q 5
       
       # p <- ggplot(data = dim_Means, aes(x = reorder(eval(dim_Means[[1]]), desc(eval(dim_Means[[1]]))), y = Means, fill = eval(dim_Means[[1]]))) +     #, fill = eval(demo_vars_hor), alpha=5)) + 
       #   geom_col()+ coord_flip(ylim = y_range) + 
       #   guides(alpha=FALSE) + theme_cowplot() +
       #   facet_grid(Items ~ ., switch = "y") +
       #   geom_text(aes(label = round(Means, 0)), hjust = -.3, size = eval(geom_text_size), family = "Gotham-Medium") + 
       #   scale_fill_manual(values = demo_colors[[1]][[2]]) +
       #   demo_dimension_theme +
       #   guides(fill =  guide_legend(ncol = numcol_legend))
       
       # p <-  ggplot(data = dim_Means, aes(x = Items, Means, fill = dim_Means[[1]])) +
       #   geom_bar(position=position_dodge(width = .8), stat="identity") +
       #   guides(fill =  guide_legend(ncol = numcol_legend)) +
       #   theme_cowplot() + 
       #   theme(legend.position = "bottom", legend.title = element_blank())+
       #   geom_text(aes(label = round(Means, 0)), vjust = -.5, size = geom_text_size, family = "Gotham-Medium", position = position_dodge(width=.8)) + 
       #   coord_cartesian(ylim = c(0, 100)) + 
       #   scale_y_continuous(breaks = c(0, 50, 100)) +
       #   theme(plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
       #         axis.title.x = element_blank(),
       #         axis.title.y = element_blank(),
       #         axis.text.y = element_text(family = "Gotham-Bold", face = "bold", hjust = 1),
       #         axis.text.x = element_text(size = 10, family = "Gotham-Bold", colour = "#3d3d3d", hjust = 0.5),
       #         
       #         legend.text = element_text(size = legend_text,family = "Gotham-Medium"),
       #         axis.line.x = element_blank(), 
       #         axis.ticks.x = element_blank(), 
       #         legend.margin = margin(10, 0, -10, 20)) +
       #   scale_fill_manual(values = c("#9E2A2F", "#75787B", "#333333", "#E6E0D3", "#4A6B7D", "#556B59", 
       #                                "#B17E4A", "#C65C3D", "#62486D", "#9E2A2F", "#75787B", "#333333", "#E6E0D3", "#4A6B7D"))
       
       p <- ggplot(data = dim_Means, aes(x = reorder(eval(dim_Means[[1]]), desc(eval(dim_Means[[1]]))), y = Means, fill = eval(dim_Means[[1]]))) +     #, fill = eval(demo_vars_hor), alpha=5)) + 
         geom_col()+ coord_flip(ylim = y_range) + 
         guides(alpha=FALSE) + theme_cowplot() +
         facet_grid(Items ~ ., switch = "y") +
         geom_text(aes(label = round(Means, 0)), hjust = -.3, size = eval(geom_text_size), family = "Gotham-Medium") + 
         scale_fill_manual(values = demo_colors[[1]][[2]]) +
         demo_dimension_theme +
         guides(fill =  guide_legend(ncol = numcol_legend))
      
      plot <- ggdraw(p) + 
        draw_label(paste0(eval(demo_vars_hor), ": ", eval(quadrants[[q]][[i]][1])), 
                   fontfamily = "Gotham-Bold", size = title_size, x = line_x[[1]],  y = title_y, hjust = 0) +
        draw_line(
          x = line_x,
          y = line_y,
          colour = ("#9E2A2F"), size = 2.2) +
        draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
        draw_image(paste0(file.images, quadrants[[q]][[1]], ".png"), scale = .15, x = .43, y = Dsym_y + .02) +
        draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]])
        # 
        # 
      
      
      ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), "_",demo_vars_hor, "_", eval(quadrants[[q]][[i]][1]),".pdf"), 
             plot = plot, width = 16, height = 9)
       
     } # closes dimension loop 
 
 } # Q5 LOOP  
  
  
  # extras <- c("extra2_likert_pressureSupport_1_How would you rate the support available to help you with the pressure?",
  #             "extra2_likert_pressureSupportUse_2_How would you rate your utilization of that available support?",
  #             "extra2_likert_JobSat_3_Taking everything into account, I would say BAL is a great place to work.")
  # extras_items <- c("How would you rate the support available to help you with the pressure?",
  #                   "How would you rate your utilization of that available support?",
  #                   "Taking everything into account, I would say BAL is a great place to work.")
  # 
  # for (e in 1:3) {
  #   
  #   pnum <- pnum + 1
  #   
  #   if (grepl("account", extras[e]) == TRUE) {
  #     df_plot <- df_level_filters %>%
  #       mutate(!!extras[e] := factor(get(extras[e]), levels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"), ordered = TRUE)) %>% 
  #       group_by(get(extras[e]), get(demo_vars_hor), .drop = FALSE) %>%
  #       count() %>% 
  #       #summarise(x = count()) %>% 
  #       drop_na() %>% 
  #       ungroup()  ### need to write this little doozy...then do some formatting and take the work here and put it on the horizontal one.  Then do the raw nps thing for the other one.  Then work on page numbers.  Edits...app
  #     df2 <- df_plot %>% 
  #       pivot_wider(names_from = `get(extras[e])`, values_from = n) %>% 
  #       #rowwise() %>% 
  #       mutate(total = rowSums(across(where(is.numeric)))) %>% 
  #       mutate(p_vDisagree = round(`Strongly Disagree`/total*100,0),
  #              p_Disagree = round(`Disagree`/total*100,0),
  #              p_neutral = round(`Neither Agree nor Disagree`/total*100,0),
  #              p_Agree = round(`Agree`/total*100,0),
  #              p_vAgree = round(`Strongly Agree`/total*100,0)) %>% 
  #       select(`get(demo_vars_hor)`,p_vDisagree:p_vAgree) %>% 
  #       rename(`Strongly Disagree` = "p_vDisagree",
  #              `Disagree` = "p_Disagree",
  #              `Neither Agree nor Disagree` = "p_neutral",
  #              `Agree` = "p_Agree",
  #              `Strongly Agree` = "p_vAgree") %>% 
  #       pivot_longer(cols= c(`Strongly Disagree`, Disagree, `Neither Agree nor Disagree`, Agree, `Strongly Agree`),
  #                    names_to = "rating",
  #                    values_to = "percent") %>% 
  #       mutate(rating = factor(rating, levels = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree"), ordered = TRUE))
  #   } else {
  #     df_plot <- df_level_filters %>%
  #       mutate(!!extras[e] := factor(get(extras[e]), levels = c("Very Weak", "Weak", "Neither Strong nor Weak", "Strong", "Very Strong"), ordered = TRUE)) %>% 
  #       group_by(get(extras[e]), get(demo_vars_hor), .drop = FALSE) %>%
  #       count() %>% 
  #       #summarise(x = count()) %>% 
  #       drop_na() %>% 
  #       ungroup() 
  #     df2 <- df_plot %>% 
  #       pivot_wider(names_from = `get(extras[e])`, values_from = n) %>% 
  #       #rowwise() %>% 
  #       mutate(total = rowSums(across(where(is.numeric)))) %>% 
  #       mutate(p_vweak = round(`Very Weak`/total*100,0),
  #              p_weak = round(`Weak`/total*100,0),
  #              p_neutral = round(`Neither Strong nor Weak`/total*100,0),
  #              p_strong = round(`Strong`/total*100,0),
  #              p_vstrong = round(`Very Strong`/total*100,0)) %>% 
  #       select(`get(demo_vars_hor)`,p_vweak:p_vstrong) %>% 
  #       rename(`Very Weak` = "p_vweak",
  #              `Weak` = "p_weak",
  #              `Neither Strong nor Weak` = "p_neutral",
  #              `Strong` = "p_strong",
  #              `Very Strong` = "p_vstrong") %>% 
  #       pivot_longer(cols= c(`Very Weak`, Weak, `Neither Strong nor Weak`, Strong, `Very Strong`),
  #                    names_to = "rating",
  #                    values_to = "percent") %>% 
  #       mutate(rating = factor(rating, levels = c("Very Weak", "Weak", "Neither Strong nor Weak", "Strong", "Very Strong"), ordered = TRUE))
  #   }
  # 
  #   p <- ggplot(data = df2, aes(x = reorder(.data[["get(demo_vars_hor)"]], desc(.data[["get(demo_vars_hor)"]])), y = percent, fill = .data[["get(demo_vars_hor)"]])) +     #, fill = eval(demo_vars_hor), alpha=5)) + 
  #     geom_col()+ 
  #     coord_flip(ylim = c(0, 100)) + 
  #     guides(alpha=FALSE) + 
  #     theme_cowplot()   + 
  #     facet_grid(rating ~ ., switch = "y") + 
  #     geom_text(aes(label = percent), hjust = -.3, size = 3, family = "Gotham-Medium") + 
  #     scale_fill_manual(values = demo_colors[[1]][[2]]) +
  #     demo_dimension_theme2 +
  #     guides(fill =  guide_legend(ncol = 1)) +
  #     ylab("Percentage of Responses")
  #   
  #   plot <- ggdraw(p) + 
  #     draw_label(str_wrap(toupper(extras_items[e]), width=45), 
  #                fontfamily = "Gotham-Bold", size = 25, x = line_x[[1]],  y = .92, hjust = 0) +
  #     draw_line(
  #       x = line_x,
  #       y = line_y,
  #       colour = ("#9E2A2F"), size = 2.2) +
  #     draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
  #     #draw_image(paste0(file.images, quadrants[[q]][[1]], ".png"), scale = logo_scale_coords[[q]][[1]], x = logo_scale_coords[[q]][[2]], y = logo_scale_coords[[q]][[3]]) +
  #     draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]])
  #   
  #   ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), eval(demo_vars_hor), "_", str_sub(eval(extras_items[e]), 1, 25),".pdf"),
  #          plot = plot, width = 16, height = 9)
  # }
  
  return(pnum)
 }#closes demo loop



