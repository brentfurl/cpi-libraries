demographics_np <- function(df, pnum1=pnum, fsize=50, word = "DEMOGRAPHICS", xcor=.15, ycor=.5){

pnum1 <- pnum1 + 1
  p <- ggplot() + theme(plot.background = element_rect(fill = "#9E2A2F"),
                        panel.background = element_rect(fill = "#9E2A2F")) 
  
  plot <- ggdraw(p) + 
    draw_label(word, fontfamily = "Gotham Black", size = fsize, x = xcor, y = ycor, hjust = 0, color = "white") +
    draw_label(paste0("n = ", nrow(df)), fontfamily = "Gotham Black", size = fsize, x = .82, y = .08, hjust = 0, color = "white") +
    draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) #+
    #draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
  
  
  ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "_DEMOGRAPHICS TITLE.pdf"), 
         plot = plot, width = 16, height = 9)     
  
return(pnum1)
}
  
  
  