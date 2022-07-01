top_x_page_np <- function(pnum, df_txt=df_qual,question="traditions" ) {
pnum <- pnum + 1
y1 <- .7
space_between <- .11
list_txt <- 30
df_txt <- df_txt %>% 
  filter(Section == tolower(question))
p <- ggplot() + theme(plot.background = element_rect(fill = "white"),
                      panel.background = element_rect(fill = "white")) 

#question="traditions"
if (nchar(question) > 15) {
plot_title <- ggdraw(p) + 
  draw_label(nrow(df_txt), fontfamily = "Gotham-Book", size = 930, x = .57, y = .51, hjust = 0,  color = "#DCDCDB") +
  draw_label(str_wrap(toupper(question), width = 65), fontfamily = "Gotham Ultra", size = 24, x = .05,  y = .95, hjust = 0, vjust = 1, color = "#9E2A2F") 
} else {
  plot_title <- ggdraw(p) + 
    draw_label(nrow(df_txt), fontfamily = "Gotham-Book", size = 930, x = .57, y = .51, hjust = 0,  color = "#DCDCDB") +
    draw_label("TRADITIONS", fontfamily = "Gotham Black", size = 42, x = .3,  y = .85, hjust = 1, vjust = 0, color = "#9E2A2F") +
    draw_label(paste0("TOP ", toupper(as.character(english(nrow(df_txt))))), fontfamily = "Gotham-Book", size = 30, x = .31,  y = .85, hjust = 0, vjust = 0, color = "#888887") 
}
 
plot3 <- plot_title +
  draw_label(paste0("1. ",  df_txt$Quotes[1]), fontfamily = "Gotham Black", size = list_txt, x = .15,  y = y1, hjust = 0, color = "#888887") +
  draw_label(paste0("2. ",  df_txt$Quotes[2]), fontfamily = "Gotham Black", size = list_txt, x = .15,  y = y1 - space_between, hjust = 0, color = "#888887") +
  draw_label(paste0("3. ",  df_txt$Quotes[3]), fontfamily = "Gotham Black", size = list_txt, x = .15,  y = y1 - 2*space_between, hjust = 0, color = "#888887") 

if (nrow(df_txt) == 3) {
  plot_main <- plot3
} else if (nrow(df_txt) == 5) {
  plot_main <- plot3 +
    draw_label(paste0("4. ",  df_txt$Quotes[4]), fontfamily = "Gotham Black", size = list_txt, x = .15,  y = y1 - 3*space_between, hjust = 0, color = "#888887") +
    draw_label(paste0("5. ",  df_txt$Quotes[5]), fontfamily = "Gotham Black", size = list_txt, x = .15,  y = y1 - 4*space_between, hjust = 0, color = "#888887") 
    
}


plot <- plot_main +
     
  draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) #+
  #draw_label(pnum, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[2]])

ggsave(paste0(file.plots, str_pad(pnum, 3, pad = "0"), "top", str_sub(question,1,30), ".pdf"), 
       plot = plot, width = 16, height = 9)   

return(pnum)
}