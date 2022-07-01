NOheadings_page_np <- function(change_or_best_day, range1, q1=q, df_txt = df_qual, pnum1 = pnum, quote_font=18, con_gap=.06, char_width=85) {

quadrants <- c("direction", "operations", "people", "engagement")

# range <- c()
# for (l in 1:length(range1)) {
#   range[l] <- range1[l]+ (q1-1)*length(range1)
# }
range=range1
  pnum1 <- pnum1 + 1
 
  df_txt <- df_txt %>% 
    filter(general_type == change_or_best_day)
  
  if (change_or_best_day == "change") {
    df_txt <- df_txt %>% 
      filter(Section == quadrants[q1])
  }
  
  quadrant_colors <- c("#C65C3D", "#556B59", "#4A6B7D", "#B17E4A", "#9E2A2F", "black")
  
  
  xhead <- .1
  xdot <- .1
  xquote <- .115
  
  if (change_or_best_day == "change") {qn = 1} else {qn = 2}
  
  if (nchar(df_qual$Question[qn]) < 66) {
    yhead1 <- .91} else {
      yhead1 <- .87}
 
  yquote1 <- yhead1 - con_gap
  yquote2 <- yquote1 - (floor(nchar(df_txt$Quotes[[1]])/char_width) * .038) - con_gap
if (nrow(df_txt)>2){  
  yquote3 <- yquote2 - (floor(nchar(df_txt$Quotes[[2]])/char_width) * .038) - con_gap}
if (nrow(df_txt)>3){  
  yquote4 <- yquote3 - (floor(nchar(df_txt$Quotes[[3]])/char_width) * .038) - con_gap}
  if (nrow(df_txt)>4){  
  yquote5 <- yquote4 - (floor(nchar(df_txt$Quotes[[4]])/char_width) * .038) - con_gap}
  if (nrow(df_txt)>5){  
  yquote6 <- yquote5 - (floor(nchar(df_txt$Quotes[[5]])/char_width) * .038) - con_gap}
  if (nrow(df_txt)>6){  
  yquote7 <- yquote6 - (floor(nchar(df_txt$Quotes[[6]])/char_width) * .038) - con_gap}
  if (nrow(df_txt)>7){  
  yquote8 <- yquote7 - (floor(nchar(df_txt$Quotes[[7]])/char_width) * .038) - con_gap}
  if (nrow(df_txt)>8){  
  yquote9 <- yquote8 - (floor(nchar(df_txt$Quotes[[8]])/char_width) * .038) - con_gap}
  if (nrow(df_txt)>9){  
  yquote10 <- yquote9 - (floor(nchar(df_txt$Quotes[[9]])/char_width) * .038) - con_gap}
  if (nrow(df_txt)>10){  
  yquote11 <- yquote10 - (floor(nchar(df_txt$Quotes[[10]])/char_width) * .038) - con_gap}
  if (nrow(df_txt)>11){  
  yquote12 <- yquote11 - (floor(nchar(df_txt$Quotes[[11]])/char_width) * .038) - con_gap}

background <- ggplot() + #theme_cowplot() +
  theme(plot.background = element_rect(fill = quadrant_colors[q1]),
        panel.background = element_rect(fill = quadrant_colors[q1]))

#for (p in 1:max(df_txt$page)) { 

plot <- ggdraw(background) + 
  
  draw_label(toupper(str_wrap(df_qual$Question[qn], width = 65)), 
             fontfamily = "Gotham Ultra", size = 20, x = con_gap,  y = .95, hjust = 0, vjust=1, color = "white", alpha=.5, lineheight = 1.1) +
  draw_label("•", x = xdot, y = yquote1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
  draw_label(str_wrap(df_txt$Quotes[[range[1]]], width=char_width), 
             fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)   
  
  #----------------------------------------------------------
if (length(range) == 1) {plot <- plot} 

#-----------------------2-----------------------------------
if (length(range) > 1){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote2,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[2]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
# if (length(range) == 2) {plot <- plot2} 
#-----------------------3-----------------------------------
if (length(range) > 2){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote3,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[3]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote3, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 3) {plot <- plot3} 
#------------------------4----------------------------------
if (length(range) > 3){
  plot4 <- plot3 +
    draw_label("•", x = xdot, y = yquote4,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[4]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote4, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
if (length(range) == 4) {plot <- plot4} 
#----------------------5------------------------------------
if (length(range) > 4){
  plot5 <- plot4 +
    draw_label("•", x = xdot, y = yquote5,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[5]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote5, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
if (length(range) == 5) {plot <- plot5} 
#----------------------6------------------------------------
if (length(range) > 5){
  plot6 <- plot5 +
    draw_label("•", x = xdot, y = yquote6,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[6]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote6, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
if (length(range) == 6) {plot <- plot6} 
#----------------------------------------------------------
if (length(range) > 6){
  plot7 <- plot6 +
    draw_label("•", x = xdot, y = yquote7,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[7]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote7, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
if (length(range) == 7) {plot <- plot7} 
#----------------------------------------------------------
if (length(range) > 7){
  plot8 <- plot7 +
    draw_label("•", x = xdot, y = yquote8,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[8]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote8, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
if (length(range) == 8) {plot <- plot8} 
#----------------------------------------------------------
if (length(range) > 8){
  plot9 <- plot8 +
    draw_label("•", x = xdot, y = yquote9,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[9]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote9, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
if (length(range) == 9) {plot <- plot9} 
#----------------------------------------------------------
if (length(range) > 9){
  plot10 <- plot9 +
    draw_label("•", x = xdot, y = yquote10,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[10]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote10, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
if (length(range) == 10) {plot <- plot10} 
#----------------------------------------------------------
if (length(range) > 10){
  plot10 <- plot9 +
    draw_label("•", x = xdot, y = yquote11,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[[range[11]]], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote11, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
if (length(range) == 11) {plot <- plot11} 

  
plot <- plot +  
  
  
  draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) #+
  #draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 



ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), change_or_best_day,"_", q, "_.pdf"), 
       plot = plot, width = 16, height = 9) 


return(pnum1)

}
