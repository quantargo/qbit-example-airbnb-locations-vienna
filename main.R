# Example leaflet plot
library(leaflet)
library(leaflet.extras)
library(tidymodels)

# Download Inside-Airbnb dataset for Vienna
listings_url <- "http://data.insideairbnb.com/austria/vienna/vienna/2021-07-07/data/listings.csv.gz"
#download.file(listings_url, basename(listings_url))
#listings <- read.csv(gzfile(basename(listings_url)), na.strings = c("","N/A")) %>%
#  mutate(price = as.numeric(sub("$", "", price, fixed = TRUE))) %>%
#  mutate(price = log(price + 1)) %>%
#  filter(!is.na(price))
listings <- readRDS("listings.rds")

# Load trained XGBoost Model for Vienna
xg_fit <- readRDS("xg_fit.rds")

# Predict prices for dataset and calculate discount
listings <- predict(xg_fit, new_data = listings) %>%
  bind_cols(listings) %>%
  mutate(price = exp(price), price_pred = exp(.pred)) %>%
  mutate(discount = price/price_pred -1) %>%
  select(name, price, price_pred, discount, listing_url, longitude, latitude)

# Create leaflet plot to show underpriced apartements
# @param num integer; Number of apartements to show on plot
create_underpriced_plot <- function(num = 500) {
  toplist <- listings %>%
    arrange(discount) %>%
    head(num)

  leaflet(toplist) %>% 
    addTiles() %>% 
    addMarkers(
      clusterOptions = markerClusterOptions(),
      label = toplist$name,
      popup = paste0('<b>', toplist$name, '</b><br><br>Price: USD ', toplist$price, '<br>Prediction: USD ', round(toplist$price_pred, 2), '<br>Discount: ', round(toplist$discount * 100, 2), ' %<br><a href="', toplist$listing_url,'" target="_blank">LINK<a>')
    ) %>%
    addHeatmap(
      lng = ~longitude, lat = ~latitude, intensity = ~price,
      blur = 20, max = 0.05, radius = 15
  )
}

create_underpriced_plot(500)



# Remove XGBoost from Environment (no live prediction, save memory)
rm("xg_fit")
