library(leaflet)
library(geosphere)

# Data: city coordinates and routes
cities <- data.frame(
  name = c("City A", "City B", "City C"),
  lat = c(51.5074, 48.8566, 52.5200),
  lon = c(-0.1278, 2.3522, 13.4050)
)

routes <- data.frame(
  from = c(1, 2),
  to = c(2, 3)
)

# Create leaflet map
m <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = cities, ~lon, ~lat, label = ~name)

# Add routes
for (i in 1:nrow(routes)) {
  from <- cities[routes$from[i], ]
  to <- cities[routes$to[i], ]
  route_coords <- gcIntermediate(c(from$lon, from$lat), c(to$lon, to$lat), addStartEnd = TRUE, sp = TRUE)
  route_coords <- route_coords@lines[[1]]@Lines[[1]]@coords
  m <- addPolylines(m, lng = route_coords[,1], lat = route_coords[,2])
}

m

route_coords@lines[[1]]@Lines[[1]]@coords


