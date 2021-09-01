# Example leaflet plot
library(leaflet)
library(leaflet.extras)
library(tidymodels)

# Download Airbnb dataset Vienna
listings_url <- "http://data.insideairbnb.com/austria/vienna/vienna/2021-07-07/data/listings.csv.gz"
#download.file(listings_url, basename(listings_url))
# listings <- read.csv(gzfile(basename(listings_url)), na.strings = c("","N/A")) %>%
#   mutate(price = as.numeric(sub("$", "", price, fixed = TRUE))) %>%
#   mutate(price = log(price + 1)) %>%
#   filter(!is.na(price))

listings <- readRDS("listings.rds")
xg_fit <- readRDS("xg_fit.rds")

listings_sel <- predict(xg_fit, new_data = listings) %>%
  bind_cols(listings) %>%
  mutate(price = exp(price), price_pred = exp(.pred)) %>%
  mutate(discount = price/price_pred -1) %>%
  select(name, price, price_pred, discount, listing_url, longitude, latitude) %>%
  arrange(discount) %>%
  head(500)

listings_sel

leaflet(listings_sel) %>% 
  addTiles() %>% 
  addMarkers(
    clusterOptions = markerClusterOptions(),
    label = listings_sel$name,
    popup = paste0('<b>', listings_sel$name, '</b><br><br>Price: $', listings_sel$price, '<br>Prediction: $', round(listings_sel$price_pred, 2), '<br>Discount: ', round(listings_sel$discount * 100, 2), ' %<br><a href="', listings_sel$listing_url,'" target="_blank">LINK<a>')
  ) %>%
  addHeatmap(
    lng = ~longitude, lat = ~latitude, intensity = ~price,
    blur = 20, max = 0.05, radius = 15
  )
