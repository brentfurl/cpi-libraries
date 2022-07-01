TOC_np <- function(lines = lines_toc, pnum1= pnum, appen_font=22, start_1st = .76, space_between=.074, x_coord=.1){


pnum1 <- pnum1 + 1

p <- ggplot() + theme(plot.background = element_rect(fill = "#9E2A2F"),
                      panel.background = element_rect(fill = "#9E2A2F")) 

plot <- ggdraw(p) + 
  draw_label("TABLE OF CONTENTS", fontfamily = "Gotham Black", size = 50, x = x_coord, y = .87, hjust = 0, color = "white") +
  draw_label(paste0("1. ",toupper(lines[1])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord,  y = start_1st, hjust = 0, color = "white") 

if (length(lines) > 1) {
  plot <- plot + draw_label(paste0("1. ",toupper(lines[2])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord,  y = start_1st - 1*space_between, hjust = 0, color = "white")}

if (length(lines) > 2) {
  plot <- plot + draw_label(paste0("2. ",toupper(lines[3])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 2*space_between, hjust = 0, color = "white")} 
if (length(lines) > 3) {
  plot <- plot + draw_label(paste0("3. ",toupper(lines[4])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 3*space_between, hjust = 0, color = "white") }
if (length(lines) > 4) {
  plot <- plot + draw_label(paste0("4. ",toupper(lines[5])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 4*space_between, hjust = 0, color = "white")}
if (length(lines) > 5) {
  plot <- plot + draw_label(paste0("5. ",toupper(lines[6])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 5*space_between, hjust = 0, color = "white")}
if (length(lines) > 6) {
  plot <- plot + draw_label(paste0("6. ",toupper(lines[7])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 6*space_between, hjust = 0, color = "white")}
if (length(lines) > 7) {
  plot <- plot + draw_label(paste0("7. ",toupper(lines[8])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 7*space_between, hjust = 0, color = "white")}
if (length(lines) > 8) {
  plot <- plot + draw_label(paste0("8. ",toupper(lines[9])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 8*space_between, hjust = 0, color = "white")} 
if (length(lines) > 9) {
  plot <- plot + draw_label(paste0("9. ",toupper(lines[10])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 9*space_between, hjust = 0, color = "white")} 
if (length(lines) > 10) {
  plot <- plot + draw_label(paste0("10. ",toupper(lines[11])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 10*space_between, hjust = 0, color = "white")} 
if (length(lines) > 11) {
  plot <- plot + draw_label(paste0("11. ",toupper(lines[12])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 11*space_between, hjust = 0, color = "white")} 
if (length(lines) > 12) {
  plot <- plot + draw_label(paste0("12. ",toupper(lines[13])), fontfamily = "Gotham-Book", size = appen_font, x = x_coord, y = start_1st - 11*space_between, hjust = 0, color = "white")} 

plot <- plot +
  draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) #+
  #draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 


ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "Table of Contents.pdf"), 
       plot = plot, width = 16, height = 9) 

return(pnum1)
}