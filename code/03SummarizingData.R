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

summarydata <- df %>%  
  mutate(Sr_ratio = round(mean(Sr_ratio), 5)) 

# Summarizing Material Type -----------------------------------------------

table(df$material_type_group) # four types of simplified material in France

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
# Figure 2 Combined count and world map -----------------------------------

df %>%
  ggplot() +
  geom_bar(aes(x = fct_infreq(material_type_group))) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "group") + 
  theme_classic()

df %>%
  ggplot(aes(x = Sr_ratio)) +
  geom_histogram(binwidth = 0.001, fill = 'midnightblue') +
  #geom_density(fill = 'midnightblue', alpha = 0.6) +
  labs(x = expression(paste(""^{87},"Sr/"^86,"Sr")), 
       y = "Count") +
  #xlim(0.7, 0.85) + 
  theme_classic()
ggsave("output/FigureHistogram.png", width = 7, height = 5, units = c("in"), dpi = 300)



# World Map ---------------------------------------------------------------
w <- world(path=tempdir())
v <- vect(df, geom= c("collection_decimal_longitude", "collection_decimal_latitude"), 
          crs = "")
crs(v) = crs(w)

plot(w)
plot(v, add = T)

B <- ggplot() +
  geom_spatvector(data = w, fill = 'grey90') + 
  geom_spatvector(data = v, aes(color = material_type_group), size = 0.6) +
  #scale_color_viridis(option = "turbo", discrete = T) + 
  scale_color_manual(values = cols, 
                     name = '') +
  theme_light() + 
  theme(legend.position = "bottom", 
        #panel.background = element_rect(fill = 'grey20')
  ) + 
  theme(legend.position = 'none') + 
  ylim(-55, 83.5) +
  guides(colour = guide_legend(override.aes = list(size = 2)))
#ggsave("output/FigureGlobalMap.png", width = 7, height = 5, units = c("in"), dpi = 300)

A <- df %>%
  ggplot(aes(x = fct_infreq(material_type_group), fill = material_type_group)) +
  geom_bar() +
  scale_fill_manual(values = cols, 
                    name = '') +
  labs(x = "Group", 
       y = 'Count', 
       fill = "Material Type (simplified)") + 
  theme_classic() + 
  # theme(axis.text.x = element_text(angle = -20, nudge_y = -0.8, hjust = 1)) + 
  NULL

ggarrange(DensPlot_Sr, B, nrow = 2, labels = "AUTO")
ggsave("output/FigureBarMap.png", width = 10, height = 10, units = c("in"), dpi = 300)