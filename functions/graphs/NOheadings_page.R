NOheadings_page <- function(gen_ques_type, range1=NULL, q1=q, df_txt = df_qual, pnum1 = pnum, quote_font=18, yheadmanual = NULL, con_gap=.06, char_width=95,color_manual=NULL,
                            institute = NULL, inst_scale = NULL, institute_x = NULL, institute_y = NULL, wrap_wid = 66, line_cons = .038, xquote = .1,
                            char_width_buffer1 = 10,
                            char_width_buffer2 = 10,
                            char_width_buffer3 = 10,
                            char_width_buffer4 = 10,
                            char_width_buffer5 = 10,
                            char_width_buffer6 = 10,
                            char_width_buffer7 = 10,
                            char_width_buffer8 = 10,
                            char_width_buffer9 = 10,
                            char_width_buffer10 = 10,
                            char_width_buffer11 = 10,
                            char_width_buffer12 = 10) {

quadrants <- c("direction", "operations", "people", "engagement")

# range1 <- c(5,2,3,1)
# df_txt <- read_sheet("https://docs.google.com/spreadsheets/d/15r7VGpGJfduEP44bW0Z8NRNnwc3XaenLxkhdU-4VyAg/edit#gid=2008154621", sheet = "df_qual")

# gen_ques_type <- "rethink"
# pnum1=1000
# quote_font=18
# con_gap=.06
# char_width=95
# q1=0
gen_ques_type <- tolower(gen_ques_type)
df_txt_all <- df_txt %>% 
  mutate(Question_type = tolower(Question_type),
         general_type = tolower(general_type),
         Section = tolower(Section))
df_questions <- df_txt[[1]]
#range=range1
  pnum1 <- pnum1 + 1
 
  df_txt <- df_txt %>% 
    mutate(Question_type = tolower(Question_type),
           general_type = tolower(general_type),
           Section = tolower(Section))
  
  if (gen_ques_type == "change") {
    df_txt <- df_txt %>% 
      filter(Section == quadrants[q1]) 
  } else {
    df_txt <- df_txt %>% filter(general_type == gen_ques_type)
  }
  
  #range1 <- NULL
  if (is.null(range1)){
    range <- 1:nrow(df_txt)
  } else {
    range <- range1
  }
  
df_txt <- df_txt %>% 
  slice(range)
  
  quadrant_colors <- c("#C65C3D", "#556B59", "#4A6B7D", "#B17E4A", "#9E2A2F", "black", "#4A6B7D")
  if (gen_ques_type == "best_day") {
    q1 <- 7
  }
  if(gen_ques_type == "change") {
    color = quadrant_colors[q1]
  } else {
    color = color_manual
  }
  
  xhead <- xquote
  xdot <- .095
  #xquote <- .1
  
  if (gen_ques_type == "change") {qn = 1} else {qn = 2}
  question <- df_txt_all %>% 
    select(Question:Question_type) %>% 
    filter(Question_type == gen_ques_type) %>% 
    select(Question) %>% 
    simplify() %>% 
    first()
  
  if (nchar(question) < wrap_wid) {
    yhead1 <- .91} else {
      yhead1 <- .87}
  
  if(!is.null(yheadmanual)) {
    yhead1 <- yheadmanual
  }
 
  yquote1 <- yhead1 - con_gap
  yquote2 <- yquote1 - (floor((nchar(df_txt$Quotes[[1]])+char_width_buffer1)/char_width) * line_cons) - con_gap
if (nrow(df_txt)>2){  
  yquote3 <- yquote2 - (floor((nchar(df_txt$Quotes[[2]])+char_width_buffer2)/char_width) * line_cons) - con_gap}
if (nrow(df_txt)>3){  
  yquote4 <- yquote3 - (floor((nchar(df_txt$Quotes[[3]])+char_width_buffer3)/char_width) * line_cons) - con_gap}
  if (nrow(df_txt)>4){  
  yquote5 <- yquote4 - (floor((nchar(df_txt$Quotes[[4]])+char_width_buffer4)/char_width) * line_cons) - con_gap}
  if (nrow(df_txt)>5){  
  yquote6 <- yquote5 - (floor((nchar(df_txt$Quotes[[5]])+char_width_buffer5)/char_width) * line_cons) - con_gap}
  if (nrow(df_txt)>6){  
  yquote7 <- yquote6 - (floor((nchar(df_txt$Quotes[[6]])+char_width_buffer6)/char_width) * line_cons) - con_gap}
  if (nrow(df_txt)>7){  
  yquote8 <- yquote7 - (floor((nchar(df_txt$Quotes[[7]])+char_width_buffer7)/char_width) * line_cons) - con_gap}
  if (nrow(df_txt)>8){  
  yquote9 <- yquote8 - (floor((nchar(df_txt$Quotes[[8]])+char_width_buffer8)/char_width) * line_cons) - con_gap}
  if (nrow(df_txt)>9){  
  yquote10 <- yquote9 - (floor((nchar(df_txt$Quotes[[9]])+char_width_buffer9)/char_width) * line_cons) - con_gap}
  if (nrow(df_txt)>10){  
  yquote11 <- yquote10 - (floor((nchar(df_txt$Quotes[[10]])+char_width_buffer10)/char_width) * line_cons) - con_gap}
  if (nrow(df_txt)>11){  
  yquote12 <- yquote11 - (floor((nchar(df_txt$Quotes[[11]])+char_width_buffer11)/char_width) * line_cons) - con_gap}

background <- ggplot() + #theme_cowplot() +
  theme(plot.background = element_rect(fill = color),
        panel.background = element_rect(fill = color))

#for (p in 1:max(df_txt$page)) { 

plot <- ggdraw(background) + 
  
  draw_label(toupper(str_wrap(question, width = wrap_wid-1)), 
             fontfamily = "Gotham Ultra", size = 20, x = con_gap,  y = .95, hjust = 0, vjust=1, color = "white", alpha=.5, lineheight = 1.1) +
  draw_label("•", x = xdot, y = yquote1,colour = ("white"), size = quote_font, color = "white", vjust=1) +
  draw_label(str_wrap(df_txt$Quotes[1], width=char_width), 
             fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote1, hjust = 0, color = "white", lineheight = 1.1, vjust = 1)   
  
  #----------------------------------------------------------
#if (length(range) == 1) {plot <- plot} 

#-----------------------2-----------------------------------
if (length(range) > 1){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote2,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[2], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote2, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
# #if (length(range) == 2) {plot <- plot} 
#-----------------------3-----------------------------------
if (length(range) > 2){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote3,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[3], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote3, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 3) {plot <- plot} 
#------------------------4----------------------------------
if (length(range) > 3){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote4,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[4], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote4, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 4) {plot <- plot} 
#----------------------5------------------------------------
if (length(range) > 4){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote5,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[5], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote5, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 5) {plot <- plot} 
#----------------------6------------------------------------
if (length(range) > 5){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote6,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[6], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote6, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 6) {plot <- plot} 
#----------------------------------------------------------
if (length(range) > 6){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote7,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[7], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote7, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 7) {plot <- plot} 
#----------------------------------------------------------
if (length(range) > 7){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote8,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[8], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote8, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 8) {plot <- plot} 
#----------------------------------------------------------
if (length(range) > 8){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote9,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[9], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote9, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 9) {plot <- plot} 
#----------------------------------------------------------
if (length(range) > 9){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote10,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[10], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote10, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 10) {plot <- plot} 
#----------------------------------------------------------
if (length(range) > 10){
  plot <- plot +
    draw_label("•", x = xdot, y = yquote11,colour = ("white"), size = quote_font, color = "white", vjust=1) +
    draw_label(str_wrap(df_txt$Quotes[11], width=char_width), 
               fontfamily = "Gotham-Book", size = quote_font, x = xquote,  y = yquote11, hjust = 0, color = "white", lineheight = 1.1, vjust = 1) 
}
#if (length(range) == 11) {plot <- plot} 

  

if (institute == "DCI" | is.null(institute) == TRUE) {
  plot <- plot +
    draw_image(paste0(file.images, "pagenum.png"), scale = Dsym_scale, x = Dsym_x, y = Dsym_y) +
    draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
} else {
  plot <- plot + 
    draw_image(institute, scale = inst_scale, x = institute_x, y = institute_y) 
    #draw_label(pnum1, fontfamily = "Gotham-Bold", size = pnum_size, x = pnum_x, y = pnum_y, hjust = 0, color = pnum_color[[1]]) 
}

# inst_scale = NULL, institute_x = NULL, institute_y = NULL, 

ggsave(paste0(file.plots, str_pad(pnum1, 3, pad = "0"), "_", gen_ques_type,"_", range[1], "to", range[length(range)], "_.pdf"), 
       plot = plot, width = 16, height = 9) 


return(pnum1)

}
