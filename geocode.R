# Aim: geocode metro stations
library(leaflet)
library(ggmap)

s = readr::read_csv("Metro_station_score.csv")
s$full_name = paste(s$Station, "Rail station, UK")

register_google("your_key")
g = ggmap::geocode(s$full_name)
# g_halifax = ggmap::geocode("halifax rail station uk")
# g[grepl(pattern = "Halifax", x = s$full_name), ] = g_halifax
summary(g$lon)
s$full_name[which.min(g$lon)] = "Sandal & Agbrigg Rail station, wakefield, UK"
g[which.min(g$lon), ] = geocode("WF2 6AE")
s$full_name[which.min(g$lat)] = "Bramley Rail station, yorkshire, UK"
g[which.min(g$lat), ] = geocode("Bramley Rail station, yorkshire, UK")


s$lon = g$lon
s$lat = g$lat

s_sf = sf::st_as_sf(s, coords = c("lon", "lat"))
leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = s_sf)

sf::write_sf(s_sf, "stations.geojson")
