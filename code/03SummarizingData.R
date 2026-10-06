# let's have a look at the output we created for France. In this script, I've set up 
# the code to not need scripts 01 or 02 ahead of time. 


# Setup -------------------------------------------------------------------

library(dplyr); library(tidyr); library(ggplot2); library(forcats); 
library(geodata); library(tidyterra); library(terra); library(ggpubr); library(viridis)

f <- vect("output/FranceSr.shp")

foutline <- world(path=tempdir()) # pull world map using geodata package
foutline <- subset(w, w$NAME_0 == "France") #there's a lot of ways to do this. Since the world map has country names, 
# we can just use the name. If you had a specific area in mind, get that shapefile and bring it into R. Make sure 
# it's the correct projection. 

# since we saved the France data in the same projection/extension as the geodata map, we should be fine. Can 
# call the vectors to double-check. 

df <- read.csv("output/FranceSr.csv")


# Basic Descriptive Information -------------------------------------------

#is it really that useful for baseline information? I still like having it
round(mean(df$Sr_ratio), 5) 
round(sd(df$Sr_ratio), 5)
round(max(df$Sr_ratio), 5) 
round(min(df$Sr_ratio), 5) 

# Summarizing Material Type -----------------------------------------------

table(df$material_type_group) # three types of simplified material in France (we know this already, we chose these)

ggplot() + 
  geom_density_ridges(data = density, aes(x = Sr_ratio, y = material_type_group, fill = material_type_group)) +
  geom_point(data = subset(density, Sr_ratio >0.9), aes(x = Sr_ratio, y = material_type_group), 
             shape = '|', size = 3, alpha = 0.7) + 
  scale_fill_viridis(option = "mako", discrete = T, name = '') +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  labs(
    x = expression(paste(''^87, "Sr/"^86, "Sr"))
  ) 


ggsave("output/FigureDensityRidges.pdf", width = 9, height = 6, units = c("in"), dpi = 300)
