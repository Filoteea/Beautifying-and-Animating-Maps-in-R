##%######################################################%##
#                                                          #
####                Data wrangling challenge            ####
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
dispersion <- read.csv("particle_dispersion.csv")

# Explore data
head(dispersion)
summary(dispersion)
str(dispersion)

# Plot a basic map ----
(basic_map <- ggplot(data = map_data("world", region = "UK")) +  
   geom_map(map = map_data("world", region = "UK"),
            aes(long, lat, map_id = region))+
   geom_point(data = dispersion,    
              aes(y = lat_coord, x = long_coord)))

#ggsave("basic_map.png")

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


# Add theme to basic map ----
(basic_map_theme <- ggplot(data = map_data("world", region = "UK")) +  
   geom_map(map = map_data("world", region = "UK"),
            aes(long, lat, map_id = region))+
   geom_point(data = dispersion,    
              aes(y = lat_coord, x = long_coord)) +
   map_theme())


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
    geom_point(data = dispersion,    # create geom points 
               aes(y = lat_coord, x = long_coord,    # set coordinates
                   
             # set point size and colour by pollutant concentration (ppt)
                   size = concentration,     
                   fill = concentration),     
               alpha = 0.5,    # set transparency  
               shape = 21) +    # set shape 
   
   # set colourblind friendly colour palette for geometric points 
    scale_fill_continuous(name = "Element X concentration (ppt)", 
                          type = "viridis", 
                          guide = "legend") +   # combine size and colour legends
    map_theme() +    # add map theme
    labs(x = "Longitude (°)",
         y = "Latitude (°)",
         size = "Element X concentration (ppt)",
         title = "Element X atmospheric distribution over UK"))   # set title 

# Create animated map ----
animated_map <- static_map +
  # set animation transition according to hour
  transition_manual(hour, cumulative = FALSE) +
  
  labs(subtitle = "{current_frame}:00:00 01/12/2021\n\n") +   # add changing hours 
  theme(legend.position = c(0.18, 0.12),   # change legend position
        legend.title = element_text(size = 17))     # change title size
  
 
 #EXTRA comment out these lines----

#animated_map <- animated_map +
  # "zoom in the map" according to the Cartesian coordinate system (add +)
 # coord_cartesian(xlim = c(-4, 0), ylim = c(54, 57)) +
  # add Edinburgh on map (add +)
  #geom_point(aes(x = -3.2053387, y = 55.9412083), size = 4, shape = 23, fill = "#660610")

# Animate! ----
animate(animated_map,    # select gganim object
        duration = 8,    # choose animation duration
        fps = 20,    # number of frames per second
        width = 800, height = 800, 
        renderer = gifski_renderer())    # choose renderer function

# Save animated map as GIF ----
# anim_save("animation_zoomed.gif")    # save animation GIF 










