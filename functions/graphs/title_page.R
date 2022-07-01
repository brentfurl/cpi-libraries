

title_page <- function(word, pnum1=pnum, color_back=deutser_red, color_word="white", fsize=50, xcor=.15, page_font ="Gotham-Bold", title_font="Gotham Black", ycor=.5, interval=.1, file_type=".pdf", df=NULL, institute = NULL, page_symbol = "yes", imagename = "NULL") {
  pnum1 <- pnum1 + 1
  p <- ggplot() + theme(plot.background = element_rect(fill = color_back),
                        panel.background = element_rect(fill = color_back)) 
  
  plot <- ggdraw(p) +
    draw_label(toupper(word[1]), fontfamily = title_font, size = fsize, x = xcor,  y = ycor, hjust = 0, vjust=.5, color = color_word) +
    draw_label(pnum1, fontfamily = page_font, size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
  
  if (page_symbol == "yes") {
  if (is.null(institute)) {
    plot <- plot +
      draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y)
  } else if (institute == "BCI"){
    plot <- plot + 
      draw_image(paste0(file.images, "BCI_pagelogo.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) 
  }
    }
  
  if (length(word) == 1) {
    plot = plot
  } 
  
  if (length(word) > 1) {
    plot <- plot +
      draw_label(toupper(word[2]), fontfamily = "Gotham Black", size = fsize, x = xcor,  y = ycor-interval, hjust = 0, color = color_word) }
  
  if (length(word) > 2) {
    plot <- plot +
      draw_label(toupper(word[3]), fontfamily = "Gotham Black", size = fsize, x = xcor,  y = ycor-2*interval, hjust = 0, color = color_word) }
  
  if (!is.null(df)) {
    plot <- plot + draw_label(paste0("n = ", nrow(df)), fontfamily = "Gotham Black", size = fsize, x = .97, y = .08, hjust = 1, color = "white") 
  }
  
  ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), word[1], "_title_", file_type), 
         plot = plot, width = 16, height = 9)    
  
  return(pnum1)
}