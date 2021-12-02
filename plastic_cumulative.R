##%######################################################%##
#                                                          #
####               Beautifying and Animating            ####
####                      maps in R                     ####
####                  Challenge Solution                ####
####                    Code written by                 ####
####                  Filoteea Moldovan                 ####
####                     01/11/2021                     ####
####                  s1819162@ed.ac.uk                 ####
####                University of Edinburgh             ####
#                                                          #
##%######################################################%##

# Install packages ----
# comment out lines to install
#install.packages("ggplot2")
#install.packages("viridis")
#install.packages("gganimate")
#install.packages("gifki")

# Libraries ----
library(ggplot2)  # data manipulation and visualization
library(viridis)    # colour-blind friendly colour scheme
library(gganimate)  # create animation
library(gifski)     # export animation as a GIF

# Import data ----
distribution <- read.csv("inputs/plastic.csv")

# Explore data
head(distribution)
summary(distribution)
str(distribution)

# Plot a basic map ----
(basic_map <- ggplot(data = map_data("world", region = "UK")) +  
   geom_map(map = map_data("world", region = "UK"),
            aes(long, lat, map_id = region))+
   geom_point(data = distribution,    
              aes(y = dec_lat_retrieve, x = dec_long_retrieve)))

# Create map theme function ----
map_theme <- function(...) {
  font <- "Helvetica"    # choose font 
  theme_minimal() +    # set minimalistic theme
    theme(
      
      # adjust axes
      axis.title = element_text(family = font,  
                                size = 22,    
                                color = "#212422"),   
      axis.line = element_line(colour = "#212422",
                               size = 0.5,
                               linetype = "solid"),
      axis.text = element_text(family = font,
                               size = 18,
                               color = "#212422",
                               margin = margin(5, b = 10)),
      axis.ticks = element_line(colour = "#212422",
                                size = 0.5,
                                linetype = "solid"),
      
      # set title further from y axis 
      axis.title.y = element_text(angle = 90, vjust = 5),     
      
      # add a subtle grid
      panel.grid.major = element_line(color = "#b8b8b8", 
                                      size = 0.2),
      panel.grid.minor = element_blank(),
      
      # remove background colors
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      
      # adjust borders and margins
      plot.margin = unit(c(.5, .5, .5, 2), 
                         "cm"),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), 
                           "cm"),
      
      # adjust titles
      legend.title = element_text(family = font, 
                                  size = 11),
      legend.text = element_text(family = font, 
                                 size = 16, 
                                 hjust = 0,
                                 color = "#222222"),
      plot.title = element_text(family = font,
                                size = 24, 
                                face = "bold",
                                hjust = 0.5,
                                color = "#222222"),
      plot.subtitle = element_text(size = 25, 
                                   hjust = 0.5,
                                   color = "#222222",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"))
    )}


# Create UK map objects ----

# create UK region dataframe
uk_map <- map_data("world", region = "UK")

# create UK map 
map_uk <- geom_map(map = uk_map,    # select UK region 
                   aes(long, lat, map_id = region),       
                   color = "#cccccc", fill = "#cccccc", size = 0.3)   

# Plot beautiful map ----
(static_map <- ggplot(data = uk_map) +    
   map_uk +    # use UK map as background
   geom_point(data = distribution,    # create geom points 
              aes(y = dec_lat_retrieve, x = dec_long_retrieve,    # set coordinates
                  
                  # set point size and colour by pollutant concentration (ppt)
                  size = sum_micro_m3,     
                  fill = sum_micro_m3),     
              alpha = 0.5,    # set transparency  
              shape = 21) +    # set shape 
   
   # set colourblind friendly colour palette for geometric points 
   scale_fill_continuous(name = "Microplastics (m^3)", 
                         type = "viridis", 
                         guide = "legend") +   # combine size and colour legends
   map_theme() +    # add map theme
   labs(x = "Longitude (°)",
        y = "Latitude (°)",
        size = "Microplastics (m^3)",
        title = "Microplastic accumulation in Scotland (m^3)"))   # set title 

# Create animated map ----
animated_map <- static_map +
  # set animation transition according to hour
  transition_manual(year, cumulative = TRUE) +
  
  labs(subtitle = "Year: {current_frame}\n\n") +   # add changing hours 
  theme(legend.position = c(0.21, 0.86),   # change legend position
        legend.title = element_text(size = 17)) +    # change title size
# "zoom in the map" according to the Cartesian coordinate system (add +)
  coord_cartesian(xlim = c(-8, 2), ylim = c(54, 62)) 

# Animate! ----
animate(animated_map,    # select gganim object
        duration = 8,    # choose animation duration
        fps = 20,    # number of frames per second
        width = 800, height = 800, 
        renderer = gifski_renderer())    # choose renderer function

# Save animated map as GIF ----
# anim_save("outputs/challenge.gif")    # save animation GIF 










