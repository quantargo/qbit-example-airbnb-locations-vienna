# Example leaflet plot
library(leaflet)
library(leaflet.extras)

# Download Airbnb dataset Vienna
listings_url <- "http://data.insideairbnb.com/austria/vienna/vienna/2021-07-07/data/listings.csv.gz"
download.file(listings_url, basename(listings_url))
listings <- read.csv(gzfile(basename(listings_url)), na.strings = c("","N/A"))

listings$price <- as.numeric(sub("$", "", listings$price, fixed = TRUE))
listings$price[is.na(listings$price)] <- median(listings$price, na.rm = TRUE)

leaflet(listings) %>% 
  addTiles() %>% 
  addMarkers(
    clusterOptions = markerClusterOptions(),
    label = listings$name,
    popup = paste0('<b>', listings$name, '</b><br><br>Price: $', listings$price, '<br><a href="', listings$listing_url,'" target="_blank">LINK<a>')
  ) %>%
  addHeatmap(
    lng = ~longitude, lat = ~latitude, intensity = ~price,
    blur = 20, max = 0.05, radius = 15
  )
