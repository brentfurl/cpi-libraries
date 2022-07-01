methods_explanation_np <- function(quant_lines=c("60 quantitative questions in 4 areas", "direction + operations + people + engagement", "5 quantitative questions about positivity", "employee net promoter score"),
                               qual_lines=c("qualitative responses - recommend one change + ", "describe your best day"), 
                               pnum1=pnum, fsize=50){

pnum1 <- pnum1 + 1
p <- ggplot() + theme(plot.background = element_rect(fill = "white"),
                      panel.background = element_rect(fill = "white"))

plot <- ggdraw(p) +
  draw_image(paste0(file.images, "methodology graphics.png"), scale = .7, x = 0, y = .1) 

if (length(qual_lines) > 0) {
  plot <- plot +
  draw_label(qual_lines[1], fontfamily = "Gotham Thin", size = 18, x = .27, y = .32, hjust = .5, color = "black") 
}

if (length(qual_lines) > 1) {
  plot <- plot +
    draw_label(qual_lines[2], fontfamily = "Gotham Thin", size = 18, x = .27, y = .32-.035, hjust = .5, color = "black") 
}

if (length(qual_lines) > 2) {
  plot <- plot +
    draw_label(qual_lines[3], fontfamily = "Gotham Thin", size = 18, x = .27, y = .32-2*.035, hjust = .5, color = "black") 
}

if (length(qual_lines) > 3) {
  plot <- plot +
    draw_label(qual_lines[4], fontfamily = "Gotham Thin", size = 18, x = .27, y = .32-3*.035, hjust = .5, color = "black") 
}

plot <- plot +
  draw_label(quant_lines[1], fontfamily = "Gotham Thin", size = 18, x = .695, y = .28, hjust = .5, color = "black") +
  draw_label(quant_lines[2], fontfamily = "Gotham Thin", size = 18, x = .695, y = .28-.035, hjust = .5, color = "black") +
  draw_label(quant_lines[3], fontfamily = "Gotham Thin", size = 18, x = .695, y = .28-2*.035, hjust = .5, color = "black") +
  draw_label(quant_lines[4], fontfamily = "Gotham Thin", size = 18, x = .695, y = .28-3*.035, hjust = .5, color = "black") 
  
plot <- plot + 
  
  draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) #+
  #draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]]) 


ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "_Methods Explanation_test.pdf"), 
       plot = plot, width = 16, height = 9)     

return(pnum1)
}

  
  
  
  