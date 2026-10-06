#run through all of 01Setup.R first, to load packages and data. 

# World Map ---------------------------------------------------------------

w <- world(path=tempdir()) # pull world map using geodata package
v <- vect(df, geom= c("collection_decimal_longitude", "collection_decimal_latitude"), 
          crs = "")
crs(v) = crs(w)

plot(w)
plot(v, add = T) #note: Stantis shortens 'TRUE' to T because she's lazy

# what countries? ---------------------------------------------------------

# What countries are represented in this dataset? 

# gotta make SpatVector w into a SpatRaster first
wRast = rast(w, nrow = 20000, ncol = 20000)
wRast = rasterize(w, wRast, field = "NAME_0")
v$country <- extract(wRast$NAME_0, v, ID = F)

countriescount <- as.data.frame(v) %>% 
  select(country)
countriescount$country <- as.character(countriescount$country)

count(dplyr::distinct(countriescount, country))

#not necessary, but I think it's interesting
countriescount %>% 
  filter(!is.na(country)) %>% 
  count(country) %>% 
  mutate(country = reorder(country, n)) %>% 
  top_n(10) %>% 
  ggplot(aes(y = country, x = n)) +
  geom_col(fill = "midnightblue") +
  theme_minimal()


france <- subset(w, w$NAME_0 == "France") #there's a lot of ways to do this. Since the world map has country names, 
# we can just use the name. If you had a specific area in mind, get that shapefile and bring it into R. Make sure 
# it's the correct projection. 


plot(france)
france_data <- mask(v, france)
plot(france_data, add = T)

# Being Selective ---------------------------------------------------------
# let's choose only sample types that make sense to us. 

table(france_data$material_type_simp)

table(france_data$migration)

france_data <- filter(france_data, !france_data$migration %in% c("large", "medium") & #excluding these two categories
                        france_data$material_type_group != "soil") #excluding anything like soils

table(france_data$material_type_simp) # checking to see what's left

plot(france)
plot(france_data, add = T)

# Save Data ---------------------------------------------------------------

# you can do this a couple ways, as a .csv or as a .shp
write.csv(as.data.frame(france_data, geom = "XY"), "output/FranceSr.csv")

writeVector(france_data, "output/FranceSr.shp", overwrite = T) 