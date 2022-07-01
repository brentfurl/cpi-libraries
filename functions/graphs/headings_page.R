headings_page <- function(gen_ques_type, heading_range, q1=q, df_txt = df_qual, pnum1 = pnum, quote_font=17,qn = FALSE,
                          con_gap=.06, char_width=95,con_gap1=.05,con_gap_new_head = .08, heading_font=20,color_manual=FALSE,
                          question_width = 75, question_font = 20, yheadmanual = NULL,
                          char_width_buffer1 = 10,
                          char_width_buffer2 = 10,
                          char_width_buffer3 = 10,
                          char_width_buffer4 = 10,
                          char_width_buffer5 = 10,
                          char_width_buffer6 = 10,
                          char_width_buffer7 = 10,
                          char_width_buffer8 = 10,
                          char_width_buffer9 = 10,
                          char_width_buffer10 = 10,
                          char_width_buffer11 = 10,
                          char_width_buffer12 = 10,
                          char_width_buffer13 = 10,
                          char_width_buffer14 = 10,
                          char_width_buffer15 = 10,
                          char_width_buffer16 = 10,
                          char_width_head_buffer2=10,
                          char_width_head_buffer3=10){
  
  # char_width_buffer1 = 10
  # char_width_buffer2 = 10
  # char_width_buffer3 = 10
  # char_width_buffer4 = 10
  # char_width_buffer5 = 10
  # char_width_buffer6 = 10
  # char_width_buffer7 = 10
  # char_width_buffer8 = 10
  # char_width_buffer9 = 10
  # char_width_buffer10 = 10
  # char_width_buffer11 = 10
  # char_width_buffer12 = 10
  # char_width_head_buffer2=10
  
  
  
 #  df_qual <- read_sheet("https://docs.google.com/spreadsheets/d/1CxO2fmAMnqqmOq4YZkcQGjXkgAZS0-FMqvQ8OSHdNKk/edit#gid=0")

  # gen_ques_type <- "change"
  # heading_range <- 1:2
  # df_txt <- df_qual
  # heading_range <- c(1:2)
  #   change_or_best_day <- "change"
  #   pnum1=1000
  #  quote_font=18
  #  char_width=95
  #  q1=1
  #  con_gap=.05
  #  con_gap1=.05
  #  con_gap_new_head = .08
  #   color_manual="#C65C3D"
  
  
  gen_ques_type <- tolower(gen_ques_type)
  df_txt_all <- df_txt %>% 
    mutate(Question_type = tolower(Question_type),
           general_type = tolower(general_type),
           Section = tolower(Section))
  df_txt <- df_txt %>% 
    mutate(Question_type = tolower(Question_type),
           general_type = tolower(general_type),
           Section = tolower(Section))
  heading_range_ab <- c(1:length(heading_range))
  
  df_questions <- df_txt[[1]] 
  quadrants_text <- c("direction", "operations", "people", "engagement", "best_day")
  
  #char_width =85
  pnum1 <- pnum1 + 1
  
  df_txt <- df_txt %>% 
    filter(general_type == gen_ques_type)
  
#  if (tolower(gen_ques_type) == "change"){#| change_or_best_day == "other") {
    df_txt <- df_txt %>% 
      mutate(Section = tolower(Section))  
  if (tolower(gen_ques_type) == "change") {  
    df_txt <- df_txt %>%  filter(Section == quadrants_text[q1])
  }
  
  headings <- unique(df_txt$Heading)[heading_range]
  df_txt <- df_txt %>% 
    filter(Heading %in% headings)
  
  quadrant_colors <- c("#C65C3D", "#556B59", "#4A6B7D", "#B17E4A", "#4A6B7D", "black", "#4A6B7D")
  
  # if(gen_ques_type == "best_day" & color_manual == FALSE) {
  #   q1 = 7
  #   color = quadrant_colors[q1]
  # } else if(gen_ques_type == "other" & color_manual == FALSE) {
  #   q1=1
  #   color = quadrant_colors[q1]
  # } else 
    
if(gen_ques_type == "change" & color_manual == FALSE) {
    color = quadrant_colors[q1]
  } else {
    color = color_manual
  }
  
  
  
  
  
  xhead <- .1
  xdot <- .13
  xquote <- .145
  
  # if (change_or_best_day == "change" & qn == FALSE) {
  #   qn = 1
  # } else if (change_or_best_day == "other" & qn == FALSE) {
  #   qn = 1} else if (change_or_best_day == "best_day" & qn == FALSE) {
  #     qn =2
  #   }
  
  question <- df_txt_all %>% 
    select(Question:Question_type) %>% 
    filter(Question_type == gen_ques_type) %>% 
    select(Question) %>% 
    simplify() %>% 
    first()
  
  #   df_txt <- read_csv("~/Dropbox/Clients/UT - comms/data/oeqs/final/qualitative.csv")
  # gen_ques_type <- "Short-term"
  
  #----------------------y coordinates 
  # if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]])) == 2) {
  
  if (nchar(question) < 66) {
    yhead1 <- .85
  } else {
    yhead1 <- .81}
  
  if (!is.null(yheadmanual)) {
    yhead1 <- yheadmanual
  }
  
  yquoteH1_1 <- yhead1 - con_gap1
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 1) {
    yquoteH1_2 <- yquoteH1_1 - (floor(
      (nchar(
        (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[1]])+char_width_buffer2
      )/(char_width)) * .038) - con_gap}
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 2) { 
    yquoteH1_3 <- yquoteH1_2 - (floor(
      (nchar(
        (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[2]])+char_width_buffer3
      )/(char_width)) * .038) - con_gap}
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 3) {
    yquoteH1_4 <- yquoteH1_3 - (floor(
      (nchar(
        (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[3]])+char_width_buffer4
      )/(char_width)) * .038) - con_gap}
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 4) {
    yquoteH1_5 <- yquoteH1_4 - (floor(
      (nchar(
        (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[4]])+char_width_buffer5
      )/(char_width)) * .038) - con_gap}
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 5) {
    yquoteH1_6 <- yquoteH1_5 - (floor(
      (nchar(
        (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[5]])+char_width_buffer6
      )/(char_width)) * .038) - con_gap}
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 6) {
    yquoteH1_7 <- yquoteH1_6 - (floor(
      (nchar(
        (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[6]])+char_width_buffer7
      )/(char_width)) * .038) - con_gap}
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 7) {
    yquoteH1_8 <- yquoteH1_7 - (floor(
      (nchar(
        (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[7]])+char_width_buffer8
      )/(char_width)) * .038) - con_gap}
  #}
  
  #-------------------------section 2 yhead
  if (length(heading_range_ab) > 1) {
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) == 1) {
      yhead2 <- yquoteH1_1 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[1]])+char_width_buffer11
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH2_1 <- yhead2 - con_gap1
    }
    else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) == 2) {
      yhead2 <- yquoteH1_2 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[2]])+char_width_buffer12
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH2_1 <- yhead2 - con_gap1
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) == 3) {
      yhead2 <- yquoteH1_3 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[3]])+char_width_buffer13
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH2_1 <- yhead2 - con_gap
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) == 4) {
      yhead2 <- yquoteH1_4 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[4]])+char_width_buffer14
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH2_1 <- yhead2 - con_gap
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) == 5) {
      yhead2 <- yquoteH1_5 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[5]])+char_width_buffer15
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH2_1 <- yhead2 - con_gap
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) == 6) {
      yhead2 <- yquoteH1_6 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[6]])+char_width_buffer16
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH2_1 <- yhead2 - con_gap
    }
    
    
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) > 1) {
      yquoteH2_2 <- yquoteH2_1 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[1]])+char_width_buffer12
        )/(char_width)) * .038) - con_gap}
    
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) > 2) {
      yquoteH2_3 <- yquoteH2_2 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[2]])+char_width_buffer13
        )/(char_width)) * .038) - con_gap
    }
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) > 3) {
      yquoteH2_4 <- yquoteH2_3 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[3]])+char_width_buffer14
        )/(char_width)) * .038) - con_gap
    }
  }
  #---------------------------------
  #-------------------------section 3 yhead
  if (length(heading_range_ab) > 2) {
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) == 1) {
      yhead3 <- yquoteH2_1 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[1]])+char_width_buffer13
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH3_1 <- yhead3 - con_gap1
    } 
    else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) == 2) {
      yhead3 <- yquoteH2_2 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[2]])+char_width_buffer14
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH3_1 <- yhead3 - con_gap1
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) == 3) {
      yhead3 <- yquoteH2_3 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[3]])+char_width_buffer15
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH3_1 <- yhead2 - con_gap
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) == 4) {
      yhead3 <- yquoteH2_4 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[4]])+char_width_buffer16
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH3_1 <- yhead3 - con_gap
    }
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) > 1) {
      yquoteH3_2 <- yquoteH3_1 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[1]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap}
    
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) > 2) {
      yquoteH3_3 <- yquoteH3_2 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[2]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap
    }
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) > 3) {
      yquoteH3_4 <- yquoteH3_3 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[3]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap
    }
  }
  #---------------------------------
  #-------------------------section 4 yhead
  if (length(heading_range_ab) > 3) {
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) == 2) {
      yhead4 <- yquoteH3_2 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[2]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH4_1 <- yhead2 - con_gap1
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) == 3) {
      yhead4 <- yquoteH3_3 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[3]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH4_1 <- yhead2 - con_gap
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) == 4) {
      yhead4 <- yquoteH3_3 -  (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[4]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap_new_head
      yquoteH4_1 <- yhead2 - con_gap
    }
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]])) > 1) {
      yquoteH4_2 <- yquoteH4_1 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]]))[["Quotes"]][[1]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap}
    
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]])) > 2) {
      yquoteH4_3 <- yquoteH4_2 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]]))[["Quotes"]][[2]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap
    }
    if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]])) > 3) {
      yquoteH4_4 <- yquoteH4_3 - (floor(
        (nchar(
          (df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]]))[["Quotes"]][[3]])+char_width_buffer3
        )/(char_width)) * .038) - con_gap
    }
  }
  #---------------------------------
  
  
  background <- ggplot() + #theme_cowplot() +
    theme(plot.background = element_rect(fill = color),
          panel.background = element_rect(fill = color)) 
  
  
  #----------------------------------
  #----------------------------------
  #----------------------------------
  #----------------------------------
  
  plot <- ggdraw(background) + 
    draw_label(toupper(str_wrap(question, width = question_width)), 
               fontfamily = "Gotham Ultra", size = question_font, x = .06,  y = .93, hjust = 0, color = "white", alpha=.5, lineheight = 1.1) +
    draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
  
  if (institute == "DCI") {
    plot <- plot +
      draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y)
  } else {
    plot <- plot + 
      draw_image(paste0(file.images, "BCI_pagelogo.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) 
  }
    #------------------------SECTION 1----------------------------------
  plot <- plot + 
    draw_label(toupper(unique(df_txt$Heading)[heading_range_ab[1]]), fontfamily = "Gotham-Medium", size = heading_font, x = xhead, y = yhead1, hjust = 0, color = "white", lineheight = 1.1) +
    draw_label("•", x = xdot, y = yquoteH1_1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[1]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 1) { 
    plot <- plot + 
      draw_label("•", x = xdot, y = yquoteH1_2, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[2]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 2) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH1_3, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[3]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_3, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) }
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 3) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH1_4, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[4]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_4, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 4) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH1_5, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[5]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_5, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 5) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH1_6, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[6]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_6, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 6) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH1_7, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[7]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_7, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]])) > 7) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH1_8, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[1]]))[["Quotes"]][[8]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_8, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  
  
  if (length(heading_range_ab) == 1) {
    
    ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "_", gen_ques_type, "_", heading_range, "_.pdf"), 
           plot = plot, width = 16, height = 9, device = "pdf") }
  
  
  #------------------------2 sections-------------------------------------------------------
  if (length(heading_range_ab) > 1) {   
    
    plot <- plot +
      draw_label(toupper(unique(df_txt$Heading)[heading_range_ab[2]]), fontfamily = "Gotham-Medium", size = heading_font, x = xhead, y = yhead2, hjust = 0, color = "white", lineheight = 1.1) +
      
      draw_label("•", x = xdot, y = yquoteH2_1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[1]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH2_1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)  } 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) > 1) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH2_2, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[2]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH2_2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) }
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) > 2) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH2_3, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[3]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH2_3, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) }
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]])) > 3) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH2_4, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[2]]))[["Quotes"]][[4]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH2_4, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  if (length(heading_range_ab) == 2) {
    
    ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "_", gen_ques_type, "_", heading_range[1],".", heading_range[2], "_.pdf"), 
           plot = plot, width = 16, height = 9, device = "pdf") }
  #--------------------------------------------------------------------------------------------------------------       
  
  #------------------------3 sections-------------------------------------------------------
  if (length(heading_range_ab) > 2) {   
    
    plot <- plot +
      draw_label(toupper(unique(df_txt$Heading)[heading_range_ab[3]]), fontfamily = "Gotham-Medium", size = heading_font, x = xhead, y = yhead3, hjust = 0, color = "white", lineheight = 1.1) +
      
      draw_label("•", x = xdot, y = yquoteH3_1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[1]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH3_1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)  } 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) > 1) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH3_2, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[2]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH3_2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) }
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) > 2) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH3_3, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[3]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH3_3, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) }
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]])) > 3) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH3_4, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[3]]))[["Quotes"]][[4]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH3_4, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  if (length(heading_range_ab) == 3) {
    
    ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "_", gen_ques_type, "_", heading_range[1],".", heading_range[2], ".", heading_range[3], "_.pdf"), 
           plot = plot, width = 16, height = 9, device = "pdf") }
  #--------------------------------------------------------------------------------------------------------------   
  
  #------------------------4 sections-------------------------------------------------------
  if (length(heading_range_ab) > 3) {   
    
    plot <- plot +
      draw_label(toupper(unique(df_txt$Heading)[heading_range_ab[4]]), fontfamily = "Gotham-Medium", size = heading_font, x = xhead, y = yhead4, hjust = 0, color = "white", lineheight = 1.1) +
      
      draw_label("•", x = xdot, y = yquoteH4_1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]]))[["Quotes"]][[1]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH4_1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)  } 
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]])) > 1) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH4_2, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]]))[["Quotes"]][[2]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH4_2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) }
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]])) > 2) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH4_3, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]]))[["Quotes"]][[3]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH4_3, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) }
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]])) > 3) {
    plot <- plot +
      draw_label("•", x = xdot, y = yquoteH4_4, colour = ("white"), size = quote_font, color = "white", vjust=1) +
      draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range_ab[4]]))[["Quotes"]][[4]], width=char_width), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH4_4, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) } 
  
  if (length(heading_range_ab) == 4) {
    
    ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "_", gen_ques_type, "_", heading_range[1],".", heading_range[2], ".", heading_range[3], ".", heading_range[4], "_.pdf"), 
           plot = plot, width = 16, height = 9, device = "pdf") }
  #--------------------------------------------------------------------------------------------------------------  
  
  return(pnum1)
  
}