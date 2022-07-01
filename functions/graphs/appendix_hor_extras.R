
appendix_hor_extras <- function(pnum, df, df_long, demo_vars_hor, dv, leg_font_dims = 9, leg_font_quad =9) {
  
  logo_scale_coords <- list(c(.11, .43, -.417), c(.173, .45, -.40), c(.11, .43, -.417), c(.173, .45, -.40), c(.15, .43, -0.417))
  # EDIT HERE:
  items <- unique(df_long %>% filter(dimension == dv) %>% pull(item))
 
  #------------------------------- GET DATASETS ------------------------------------------------  
  
  #demo_vars_hor <- "Tenure"
  
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
    filter(dimension == dv) %>% 
    group_by(get(demo_vars_hor)) %>% 
    filter(n() > 4*length(items)) %>%
    droplevels() %>%
    ungroup() %>%
    mutate(!!demo_vars_hor := as.character(get(demo_vars_hor))) %>%
    filter(!is.na(get(demo_vars_hor))) %>%
    mutate(!!demo_vars_hor :=  factor(get(demo_vars_hor))) %>%
    mutate(!!demo_vars_hor :=  fct_relevel(get(demo_vars_hor), na.omit(levels)))
  
 ##### plot
  
      level_nums <- 1:length(items)
      geom_text_size <- 3.2
      
      
        item_range <- eval(q * 15 - 14):eval(q * 15 - 10)
        #item_labels <- all_items[eval(q * 15 - 14):eval(q * 15 - 10)]
        cpi_vars <- colnames(df %>% select(contains(dv)))
      ########################################## PLOT BEGIN ###########################
      
      pnum <- pnum + 1
      item_labels_wrapped <- str_wrap(items, width = 47) # items_wrapped
      
      
      ## do this next and try to get a Adaptability values one to work
      long_dim  <- df_level_filters %>% select(demo_vars_hor, contains("belonging")) %>% gather(dim_items, Items, cpi_vars)  # long_dim, dim_items, Items, 
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
        theme(legend.text = element_text(size = leg_font_dims,family = "Gotham-Medium")) +
        guides(fill =  guide_legend(ncol = numcol_legend))
      
      plot <- ggdraw(p) + 
        draw_label(paste0(eval(demo_vars_hor), ": ", str_to_title(dv)), 
                   fontfamily = "Gotham-Bold", size = title_size, x = line_x[[1]],  y = .92, hjust = 0) +
        draw_line(
          x = line_x,
          y = line_y,
          colour = ("#9E2A2F"), size = 2.2) +
        #draw_image(paste0(file.images, quadrants[[q]][[1]], ".png"), scale = logo_scale_coords[[q]][[1]], x = logo_scale_coords[[q]][[2]], y = logo_scale_coords[[q]][[3]]) +
        draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]])
      if (institute == "DCI" | exists(institute) == FALSE) {
        plot <- plot +
          draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y)
      } else {
        plot <- plot + 
          draw_image(paste0(file.images, "BCI_pagelogo.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) 
      }
      
      if (grepl("*", paste(item_labels, collapse = ''), fixed = TRUE) == TRUE) {
        plot <- plot +
          draw_label("*", fontfamily = "Gotham-Bold", size = 14, x = .23,  y = .02, hjust = 0) +
          draw_label("reverse scored", fontfamily = "Gotham-Medium", size = 10, x = .24,  y = .02, hjust = 0)
        #draw_label("Scores for these questions are reversed so that higher scores represent better outcomes.", fontfamily = "Gotham-Book", size = 10, x = .24,  y = .02, hjust = 0)
      }
      
      
      ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), "_",demo_vars_hor, "_", eval(dv),".pdf"), 
             plot = plot, width = 16, height = 9)
      
      
  
  return(pnum)
}#closes demo loop



