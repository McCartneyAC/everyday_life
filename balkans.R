library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

# Countries you want to include
balkans <- c("Slovenia", "Croatia", "Bosnia and Herzegovina",
             "Republic of Serbia", "Montenegro", "Albania",
             "North Macedonia", "Kosovo", "Bulgaria",
             "Greece", "Romania", "Hungary", "Austria", "Italy")

world <- ne_countries(scale = "medium", returnclass = "sf")
world %>% names()
world$admin

# balkan_map <- world[world$admin %in% balkans, ]
balkan_map <- world %>% 
  filter(admin %in% balkans)

ggplot(balkan_map) +
  geom_sf(fill = "grey90", color = "grey40", size = 0.4) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(xlim = c(13, 30), ylim = c(39, 48))


# cities <- data.frame(
#   name = c("Ljubljana", "Sarajevo", "Mostar", "Belgrade",
#            "Prizren", "Skopje", "Sofia", "Plovdiv",
#            "Rila Monastery", "Thessaloniki", "Tirana", 
#            "Kotor", "Guca", "Bajina Basta", "Zagreb", "Ohrid"),
#   lon = c(14.5058, 18.4131, 17.8150, 20.4573,
#           20.7397, 21.4300, 23.3219, 24.7489,
#           23.3935, 22.9444, 19.8187, 18.7700, 20.223370,19.562618,15.969577,20.816404),
#   lat = c(46.0569, 43.8564, 43.3438, 44.7872,
#           42.2153, 41.9973, 42.6977, 42.1354,
#           42.1369, 40.6401, 41.3275, 42.4247, 43.779201,43.977057,45.812090,41.121616),
#   order = c(1,3,4,NA,NA,NA,NA,NA,NA,NA,NA,5,NA,NA,2, NA),
#   date  = c(0,0,0,0,0,0,0,0,0,0,0,0, "08-08-2026", "07-14-2026",0, 0)
# )
cities <- readr::read_csv("C:\\Users\\a.mccartney\\Desktop\\travel\\balkans.csv")
cities <- cities %>% 
  mutate(order = row_number()) %>%
  mutate(date = mdy(date)) 
  
cities


cities_sf <- st_as_sf(cities, coords = c("lon","lat"), crs = 4326)
cities_sf

# add lon/lat columns from geometry
coords <- st_coordinates(cities_sf)
cities_sf$lon <- coords[, 1]
cities_sf$lat <- coords[, 2]

ggplot() +
  geom_sf(data = balkan_map, fill = "grey90", color = "grey40", alpha = 0.57) +
  geom_sf(data = cities_sf, color = "red", size = 3) +
  geom_path(data = cities, aes(x = lon, y = lat), color = "red", linewidth = 1) +
  geom_text(data = cities_sf,
            aes(x = lon, y = lat, label = name),
            nudge_y = 0.17,
            size = 3,
            color = "black") +
  theme_minimal()+
  coord_sf(xlim = c(13, 25), ylim = c(40, 47))




