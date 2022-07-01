single_quote_page <- function(change_or_best_day, range1, q1=q, df_txt = df_qual, pnum1 = pnum, 
                              quote_font=24, line_height=1.2, char_width=50, xdot=.17, xdot_plus=.002) {

quadrants <- c("direction", "operations", "people", "engagement")


  df_txt <- df_txt %>% 
    filter(general_type == change_or_best_day)
  
  if (change_or_best_day == "change") {
    df_txt <- df_txt %>% 
      filter(Section == quadrants[q1])
  }
  
  quadrant_colors <- c("#C65C3D", "#556B59", "#4A6B7D", "#B17E4A", "#9E2A2F", "black")
  
  
  xhead <- .1
  xquote <- xdot + xdot_plus
  
  if (change_or_best_day == "change") {qn = 1} else {qn = 2}
  
  if (nchar(df_qual$Question[qn]) < 96) {
    yhead1 <- .8} else {
      yhead1 <- .76}
 
  yquote1 <- yhead1 

background <- ggplot() + #theme_cowplot() +
  theme(plot.background = element_rect(fill = quadrant_colors[q1]),
        panel.background = element_rect(fill = quadrant_colors[q1]))

for (n in 1:nrow(df_txt)) { 
pnum1=pnum1+1
plot <- ggdraw(background) + 
  
  draw_label(toupper(str_wrap(df_qual$Question[qn], width = 95)), 
             fontfamily = "Gotham Ultra", size = 18, x = con_gap,  y = .95, hjust = 0, vjust=1, color = "white", alpha=.5, lineheight = line_height) +
  draw_label(" \"", fontfamily = "Gotham-Book", x = xdot, y = yquote1,colour = ("white"), size = quote_font, color = "white", vjust=1, hjust=1) +
  draw_label(paste0(str_wrap(df_txt$Quotes[[n]], width=char_width), "\""),
             fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote1, hjust = 0, color = "white", lineheight = line_height, vjust = 1) +  
  
  #----------------------------------------------------------

  draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
  draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 



ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), change_or_best_day,"_", q, "_.pdf"), 
       plot = plot, width = 16, height = 9) 

}
return(pnum1)

}
