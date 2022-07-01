headings_page_np <- function(change_or_best_day, q1=q, heading_range, df_txt = df_qual, pnum1 = pnum, quote_font=18, head_font=20) {


quadrants_text <- c("direction", "operations", "people", "engagement", "best_day")

char_width =85
pnum1 <- pnum1 + 1

df_txt <- df_txt %>% 
  filter(general_type == change_or_best_day)

if (change_or_best_day == "change") {
  df_txt <- df_txt %>% 
    filter(Section == quadrants[q1])
}
quadrant_colors <- c("#C65C3D", "#556B59", "#4A6B7D", "#B17E4A", "#4A6B7D", "black")
 
xhead <- .1
xdot <- .13
xquote <- .145

if (change_or_best_day == "change") {qn = 1} else {qn = 2}




#----------------------y coordinates 
# if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]])) == 2) {
  if (nchar(df_qual$Question[qn]) < 66) {
    yhead1 <- .81
    } else {
    yhead1 <- .77}
  # yquoteH1_1 <- yhead1 - .06
  # yquoteH1_2 <- yquoteH1_1 - (floor(nchar((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]]))[["Quotes"]][[1]])/(char_width + 1)) * .038) - .06
#}

# if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]])) == 3) {
#   if (nchar(df_qual$Question[qn]) < 66) {
#     yhead1 <- .81
#   } else {
#     yhead1 <- .77}
#   
  yquoteH1_1 <- yhead1 - .06
  yquoteH1_2 <- yquoteH1_1 - (floor(nchar((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]]))[["Quotes"]][[1]])/(char_width + 1)) * .038) - .06
  yquoteH1_3 <- yquoteH1_2 - (floor(nchar((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]]))[["Quotes"]][[2]])/(char_width + 1)) * .038) - .06
#}


if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]])) == 2) {
  yhead2 <- yquoteH1_2 -  (floor(nchar((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]]))[["Quotes"]][[2]])/(char_width + 1)) * .038) - .06 - .04
  yquoteH2_1 <- yhead2 -.06
  } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]])) == 3) {
  yhead2 <- yquoteH1_3 -  (floor(nchar((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]]))[["Quotes"]][[3]])/(char_width + 1)) * .038) - .06 - .04
  yquoteH2_1 <- yhead2 -.06
  }

if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]])) == 2) {
  yquoteH2_2 <- yquoteH2_1 - (floor(nchar((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]]))[["Quotes"]][[1]])/(char_width + 1)) * .038) - .06
} else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]])) == 3) {
  yquoteH2_2 <- yquoteH2_1 - (floor(nchar((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]]))[["Quotes"]][[1]])/(char_width + 1)) * .038) - .06
  yquoteH2_3 <- yquoteH2_2 - (floor(nchar((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]]))[["Quotes"]][[2]])/(char_width + 1)) * .038) - .06
}
  

background <- ggplot() + #theme_cowplot() +
  theme(plot.background = element_rect(fill = quadrant_colors[q1]),
        panel.background = element_rect(fill = quadrant_colors[q1])) 


plot <- ggdraw(background) + 
  
  draw_label(toupper(str_wrap(df_qual$Question[qn], width = 65)), 
             fontfamily = "Gotham Ultra", size = 20, x = .06,  y = .90, hjust = 0, color = "white", alpha=.5, lineheight = 1.1) +
 
  #------------------------SECTION 1----------------------------------
  draw_label(toupper(unique(df_txt$Heading)[heading_range[1]]), fontfamily = "Gotham-Medium", size = head_font, x = xhead, y = yhead1, hjust = 0, color = "white", lineheight = 1.1) +
  
  draw_label("•", x = xdot, y = yquoteH1_1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
  draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]]))[["Quotes"]][[1]], width=85), 
             fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)   +
  draw_label("•", x = xdot, y = yquoteH1_2, colour = ("white"), size = quote_font, color = "white", vjust=1) +
  draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]]))[["Quotes"]][[2]], width=85), 
             fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)  
  
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]])) == 2) {
    plot <- plot +
      draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) # +
      # draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
  } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]])) == 3) {
      plot <- plot +
        draw_label("•", x = xdot, y = yquoteH1_3, colour = ("white"), size = quote_font, color = "white", vjust=1) +
        draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[1]]))[["Quotes"]][[2]], width=85), 
                   fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH1_3, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)  +
        
        draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) #+
        #draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
    }
  

if (length(heading_range) == 1) {

      ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), change_or_best_day, "_", q, "_", heading_range[1],"_.pdf"), 
       plot = plot, width = 16, height = 9) 
  
} else if (length(heading_range) == 2) {
#------------------------2 sections-------------------------------------------------------
  if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]])) == 2) {
    
      plot <- plot +
        draw_label(toupper(unique(df_txt$Heading)[heading_range[2]]), fontfamily = "Gotham-Medium", size = head_font, x = xhead, y = yhead2, hjust = 0, color = "white", lineheight = 1.1) +
    
        draw_label("•", x = xdot, y = yquoteH2_1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
        draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]]))[["Quotes"]][[1]], width=85), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH2_1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)   +
        draw_label("•", x = xdot, y = yquoteH2_2, colour = ("white"), size = quote_font, color = "white", vjust=1) +
        draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]]))[["Quotes"]][[2]], width=85), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH2_2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)  +
        
        draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) #+
        #draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
      
    } else if (nrow(df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]])) == 3) {

      plot <- plot +
        draw_label(toupper(unique(df_txt$Heading)[heading_range[2]]), fontfamily = "Gotham-Medium", size = head_font, x = xhead, y = yhead2, hjust = 0, color = "white", lineheight = 1.1) +
    
        draw_label("•", x = xdot, y = yquoteH2_1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
        draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]]))[["Quotes"]][[1]], width=85), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH2_1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)   +
        draw_label("•", x = xdot, y = yquoteH2_2, colour = ("white"), size = quote_font, color = "white", vjust=1) +
        draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]]))[["Quotes"]][[2]], width=85), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH2_2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)  
        draw_label("•", x = xdot, y = yquoteH3_3, colour = ("white"), size = quote_font, color = "white", vjust=1) +
        draw_label(str_wrap((df_txt %>% filter(Heading == unique(df_txt$Heading)[heading_range[2]]))[["Quotes"]][[3]], width=85), 
                 fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquoteH3_3, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)  +
          
        draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) #+
        #draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
        
    }

  ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "_", change_or_best_day, "_", q, "_", heading_range[1], heading_range[2], "_.pdf"), 
         plot = plot,  width = 16, height = 9) 
  
}

return(pnum1)

}
