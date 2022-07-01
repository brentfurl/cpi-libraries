

appen_title <- function(pnum1=pnum, color_back=deutser_red, color_word="white", explan_width=55,explan_fsize=18, fsize=50, xcor=.15, ycor_minus = .08, ycor=.52, interval=.1, file_type=".pdf", df=NULL, institute="DCI") {
  pnum1 <- pnum1 + 1
  explanation <- c("Category options with less than 5 respondents were not included in the appendix so that group averages are meaningful and to ensure anonymity.")
  p <- ggplot() + theme(plot.background = element_rect(fill = color_back),
                        panel.background = element_rect(fill = color_back)) 
  
  plot <- ggdraw(p) +
    draw_label(toupper("appendix"), fontfamily = "Gotham Black", size = fsize, x = xcor,  y = ycor, hjust = 0, color = color_word) +
    draw_label(str_wrap(toupper(explanation), width = explan_width), fontfamily = "Gotham-Book", size = explan_fsize, x = xcor,  y = ycor-ycor_minus, hjust = 0, color = color_word) +
    draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
  
  if (institute == "DCI" | !exists(institute)) {
    plot <- plot +
      draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y)
  } else {
    plot <- plot + 
      draw_image(paste0(file.images, "BCI_pagelogo.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) 
  }
    
  
  
  ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "appendix_title_", file_type), 
         plot = plot, width = 16, height = 9)    
  
  return(pnum1)
}