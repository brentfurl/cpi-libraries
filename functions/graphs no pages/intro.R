################## INTRO TITLE PAGE #################################
intro <- function(title, date_report=date, extra_title1 = "", main_size=85, date_size=40, ymain=.5, extra_size=38,x_ind=.13, minus_main_y=.1) {
  # title = company
  # date_report=date
  # extra_title = ""
  # main_size=85
  # date_size=40
  # ymain=.5
  # extra_size=38
  # x_ind=.13
  # minus_main_y=.1
pnum <- 1
p <- ggplot() + theme(plot.background = element_rect(fill = "#56565A"),
                      panel.background = element_rect(fill = "#56565A")) 


plot <- ggdraw(p) + 
  draw_label(toupper(title), fontfamily = "Gotham Black", size = main_size, x = x_ind, y = ymain, hjust = 0, color = "white") +
  draw_label(toupper(date_report), fontfamily = "Gotham Light", size = date_size, x = x_ind, y = .24, hjust = 0, color = "white") +
  draw_line(x = c(.001, .999), y = c(.087, .087), colour = ("white"), size = 52) +
  draw_image(paste0(file.images, "titlepage_logo.png"), scale = .11, x = 0, y = -.4)

if (extra_title1 != "") {
  plot=plot
} else {
  plot <- plot +
    draw_label(toupper(extra_title1), fontfamily = "Gotham-Book", size = extra_size, x = x_ind, y = ymain-minus_main_y, hjust = 0, color = "white") 
}


ggsave(paste0(file.plots, "001_INTRO.pdf"), 
       plot = plot, width = 16, height = 9)    
return(pnum)
}