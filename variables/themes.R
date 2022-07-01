font_add("Gotham-Bold", "GothamBold.ttf")
font_add("Gotham-Book", "GothamBook.ttf")
font_add("Gotham-Medium", "GothamMedium.ttf")
font_add("Gotham-Medium", "GothamMedium.ttf")
font_add("Gotham Ultra", "Gotham Ultra.ttf")
font_add("Gotham Thin", "Gotham-Thin.ttf")
font_add("Gotham Black", "Gotham Black 900.ttf")
font_add("Gotham Light", "Gotham-Light.ttf")
showtext_auto()

font_add("Gotham-Bold", "C:/Users/Brent/AppData/Local/Microsoft/Windows/Fonts/GothamBold.ttf")
font_add("Gotham-Book", "C:/Users/Brent/AppData/Local/Microsoft/Windows/Fonts/GothamBook.ttf")
font_add("Gotham-Medium", "C:/Users/Brent/AppData/Local/Microsoft/Windows/Fonts/GothamMedium.ttf")
showtext_auto()

################################ SET VARS  ###############################################
# file paths
#filepath <- "~/Dropbox/Clients/Ergon ultimate/make_plots/plot scripts new/All/"
# main_plot_type_paths <- c("demo cuts/", "dimensions all/", "main demo overview/")
# demo_cuts_subpaths <- c("dimension level/", "item level/")
# df vars

#demo_vars <- c("LOCATION", "TENURE", "ROLE", "GENERATION")
#demo_vars <- c("LOCATION", "TENURE", "GENERATION", "DIVISION") # for roles being df filters...MOVING THIS VARIABLE TO UI SCRIPT CALLS
#demo_vars_all <- c("All", "Location", "Tenure", "Role", "Generation")
demo_colors <- list(
  # list("Location", c("#75787B", "#828588", "#909395", "#9EA0A2", "#ACAEAF", "#BABBBD", "#C7C9CA", "#D5D6D7", "#E3E4E4", "#F1F1F1")),
  # list("Tenure", c("#333333", "#474747", "#5B5B5B", "#707070", "#848484", "#999999", "#ADADAD", "#C1C1C1", "#D6D6D6", "#EAEAEA")),
  list("Division", c("#9E2A2F", "#75787B", "#333333", "#E6E0D3", "#4A6B7D", "#556B59", "#B17E4A", "#C65C3D", "#62486D", "#9E2A2F", "#75787B", "#333333", "#E6E0D3", "#4A6B7D")))
# list("Generation", c("#4A6B7D", "#5C798A", "#6E8897", "#8097A4", "#92A6B1", "#A4B5BE", "#B6C3CB", "#C8D2D8", "#DAE1E5", "#ECF0F2")))

# all_cpi_vars <- c(colnames(df)[grep("cpi", colnames(df))],  colnames(df)[grep("positivity_", colnames(df))], 
#                   colnames(df)[grep("satisfaction_", colnames(df))],colnames(df)[grep("effort_", colnames(df))], 
#                   colnames(df)[grep("extraPlus", colnames(df))]) 


# all_items <- c("The company's vision is very clear.",	"I can summarize the company's vision to someone I meet.",	"I feel inspired by the company's vision.",	"Leaders are passionate about the company's vision.",	"Leaders are guided by the company's vision.",	"I have confidence in the company's strategy.",	"I know what my company wants to achieve in the next year.",	"This company knows how to get the results it is looking for.",	"We know what our next step is for improving our capabilities.",	"Leadership effectively connects my work to the company goals.",	"I have a great deal of confidence in the leadership of this company.",	"Company leaders impact the attitude and culture of the entire company.",	"The leadership of this company is fantastic in comparison to the leadership of other organizations I've been involved with.",	"Leaders find the time to get to know the employees.",	"I have confidence in the decision making of company leaders. ",	
#                "We deal with the same problems again and again.*",	"Company changes usually have a positive impact on me.",	"People look to pass the blame when something goes wrong.*",	"If an issue arises, people mostly remain calm and focused.",	"We use feedback from customers to improve our services.",	"I have real time access to business data.",	"My team uses performance measures (i.e. key performance indicators).",	"We are recognized for our successes.",	"We have a clear sense at all times whether or not we're meeting our objectives.",	"We are held accountable for our performance.",	"I often use 'workarounds' instead of the company's established systems.*",	"It often takes too long to get the information I need.*",	"Oftentimes, I have to take care of things that shouldn't be my responsibility.*",	"I know who to go to when I have concerns about our systems/processes.",	"I follow the company's standardized procedures.",	
#                "Our team consistently meets our goals.",	"Our work would be better if we collaborated more.",	"Our skills complement each other well.",	"There's a real sense that I am part of a team.",	"Our company has a spirit of teamwork and cooperation.",	"My company does a good job of hiring the right people for the right jobs.",	"I am confident my performance affects my compensation.",	"I get along well with my supervisor.",	"Performance reviews are a waste of time.*",	"Based on my experience, the company does a great job of acclimating new employees.",	"I get helpful feedback from my supervisor on a regular basis.",	"I am consistently improving at my job.",	"People here are always willing to help me learn.",	"The company invests in employees learning new skills.",	"I have grown professionally during my time with this company.",	
#                "My job role is always extremely clear.",	"I am very frequently overwhelmed at work.*",	"My job is a great fit for my skills and strengths.",	"I embrace the company's core values.",	"I look forward to going to work.",	"I believe in the products or services we provide.",	"I respond to customer needs as quickly as possible.",	"I build great relationships with my customers.",	"I'm disappointed if I don't meet my customers' expectations.",	"I seek feedback from customers to improve our products or services.",	"My opinions are heard and valued.",	"I am confident that leadership understands what I do.",	"I am very comfortable letting my supervisor know when I've made a mistake.",	"I trust my supervisor has my best interest in mind.",	"I feel micromanaged.*",
#                "People in my organization express genuine gratitude toward one another.","I practice mindfulness by staying in the present moment while doing my work.", "People I work with generally feel optimistic about the organization's future.", "People that I work with care about my happiness and well-being.", "I feel inspired every day to do my best work.", "Taking everything into consideration, I am satisfied with my job as a whole.", "I am willing to put in a great deal of effort beyond what is normally expected in order to help the organization.",
#                "Passion", "Accountability", "Courage", "Transparency", "Passion", "Accountability", "Courage", "Transparency")

quadrants <- list(list("Direction",c("Vision", "VISION", "vision"), c("Strategy", "STRATEGY", "strategy"), c("Leadership", "LEADERSHIP", "leadership")), 
                  list("Operations",c("Adaptability", "ADAPTABILITY"), c("Performance","PERFORMANCE","performance"), c("Systems", "SYSTEMS", "systems")), 
                  list("People",c("Teamwork","TEAMWORK"), c("Talent", "TALENT"), c("Development", "DEVELOPMENT", "development")), 
                  list("Engagement",c("Fit", "FIT"), c("Customer", "CUSTOMER"), c("Climate", "CLIMATE")),
                  list("PositivitySatEff", c("Positivity", "POSITIVITY"), c("Satisfaction + Effort", "SATISFACTION + EFFORT")))
                  


# colors

demo_overview_color <- "#9E2A2F"
quadrant_colors <- c("#C65C3D","#556B59","#4A6B7D", "#B17E4A","#9E2A2F", "black")
positivity_color <- "#E6E0D3"
SE_color <- "#75787B"


sample_overview_theme <- theme(
  plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
  axis.title.y = element_blank(),
  plot.title = element_blank(),
  axis.text.x = element_text(size = 14, vjust = -.2, family = "Gotham-Bold", face = "bold"),
  axis.line.y = element_blank(), 
  axis.ticks.y = element_blank(),
  #axis.text.y = element_text(size = 16, family = "Gotham-Bold", face = "bold"),
  axis.text.y = element_text(family = "Gotham-Bold", face = "bold", hjust = 1),
  axis.title.x = element_text(size = 14, family = "Gotham-Bold", vjust = -2, colour = "#3d3d3d")) #, face = "bold"))


dem_quad_theme <- theme(
  plot.margin = margin(1.5, 1.5, .7, 1, "inches" ),
  strip.text.y.left = element_text(size = 16, family = "Gotham-Bold", face = "bold", hjust = 1, 
                                   margin=margin(l = 50), angle = 0), 
  strip.background = element_rect(fill = "white", linetype = 1.5, size = 0), 
  strip.placement = "outside", 
  legend.title = element_blank(), 
  #legend.key.size = unit(.4, "cm"), 
  legend.position="bottom", 
  legend.direction = "vertical", 
  legend.text = element_text(size = 9, family = "Gotham-Medium"),
  # legend.spacing.x = unit(.14, "inches"),
  axis.text.x = element_blank(), 
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title = element_blank(),
  panel.spacing.y = unit(.4, "inches"))


demo_dimension_theme <- theme(
  plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
  strip.text.y.left = element_text(angle = 0, hjust = 0, size = 14, family = "Gotham-Medium", margin = margin(0,1.2,0,0, "cm")), 
  strip.background.y = element_rect(fill = NA, color=NA),
  axis.text.y = element_blank(), 
  axis.line = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = 16, family = "Gotham-Bold",face = "bold"),
  axis.title = element_blank(),
  legend.title = element_blank(),
  legend.position="bottom", 
  #legend.key.size = unit(.4, "cm"), 
  legend.direction = "vertical", 
  legend.text = element_text(size = 9,family = "Gotham-Medium"), 
  panel.grid = element_blank(),
  panel.border = element_rect(color = "#E3E4E4"))

demo_dimension_theme2 <- theme(
  plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
  strip.text.y.left = element_text(angle = 0, hjust = 0, size = 14, family = "Gotham-Bold", face = "bold", margin = margin(0,1.2,0,0, "cm")), 
  strip.background.y = element_rect(fill = NA, color=NA),
  axis.text.y = element_blank(), 
  axis.line = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = 16, family = "Gotham-Bold",face = "bold"),
  axis.title.y = element_blank(),
  axis.title.x = element_text(size = 12, family = "Gotham-Bold", colour = "#3d3d3d"),
  legend.title = element_blank(),
  legend.position="bottom", 
  #legend.key.size = unit(.4, "cm"), 
  legend.direction = "vertical", 
  legend.text = element_text(size = 9,family = "Gotham-Medium"), 
  panel.grid = element_blank(),
  panel.border = element_rect(color = "#E3E4E4"))

all_quadrant_theme <- theme(
  plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
  axis.title = element_blank(),
  axis.text.y = element_text(size = 16, family = "Gotham-Bold", face = "bold"),
  axis.line.y = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = 16, family = "Gotham-Bold",face = "bold"))

all_dimension_theme <- theme(
  plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
  axis.title = element_blank(),
  axis.text.y = element_text(hjust = 0, size = 16, family = "Gotham-Medium", face = "bold"),
  axis.line.y = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = 16, family = "Gotham-Bold",face = "bold"))

all_dimension_theme2 <- theme(
  plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
  axis.title = element_blank(),
  axis.text.y = element_text(hjust = 0, size = 14.5, family = "Gotham-Bold", face = "bold"),
  axis.line.y = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = 16, family = "Gotham-Bold",face = "bold"))

dimension_grid_theme_x <- theme(
  #plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
  axis.title = element_blank(),
  axis.text.y = element_text(hjust = 0, size = 12, family = "Gotham-Bold", face = "bold"),
  axis.line.y = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = 12, family = "Gotham-Book",face = "bold", angle = 35, hjust=1, vjust=1))

dimension_grid_theme_no.x <- theme(
  #plot.margin = margin(1.5, 1.5, .7, 1.5, "inches" ),
  axis.title = element_blank(),
  axis.text.y = element_blank(),
  axis.line.y = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_text(size = 12, family = "Gotham-Book",face = "bold", angle = 35, hjust=1, vjust=1))































