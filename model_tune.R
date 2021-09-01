library(tidymodels)
library(xgboost)

listings_url <- "http://data.insideairbnb.com/austria/vienna/vienna/2021-07-07/data/listings.csv.gz"
download.file(listings_url, basename(listings_url))
listings <- read.csv(gzfile(basename(listings_url)), na.strings = c("","N/A")) %>%
  mutate(price = as.numeric(sub("$", "", price, fixed = TRUE))) %>%
  mutate(price = log(price + 1)) %>%
  filter(!is.na(price))
  

# Remove outliers
#listings <- listings %>% 
#  filter(price < 500 & price > 10)

set.seed(42)
airbnb_split <- initial_split(listings)
airbnb_train <- training(airbnb_split)
airbnb_test <- testing(airbnb_split)
airbnb_cv <- vfold_cv(airbnb_train, 5)

#  minimum_minimum_nights + room_type + 
xg_rec <- recipe(price ~ minimum_nights + room_type + number_of_reviews + 
                latitude + longitude + neighbourhood_cleansed + reviews_per_month + 
                calculated_host_listings_count + availability_365 + last_review, 
              data = airbnb_train) %>%
  step_mutate(is_innere_stadt = neighbourhood_cleansed == "Innere Stadt") %>%
  step_rm(neighbourhood_cleansed) %>%
  step_mutate(last_review = as.integer(Sys.Date() - as.Date(last_review))) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_mutate(reviews_per_month = replace_na(reviews_per_month, 0))

xg_mod <- boost_tree(mode = "regression",
                        mtry = tune(),
                        trees = tune(),
                        learn_rate = .01) %>%
  set_engine("xgboost")

xg_wf <- workflow() %>%
  add_recipe(xg_rec) %>%
  add_model(xg_mod)

mset <- metric_set(rmse)

xg_tune <- xg_wf %>%
  tune_grid(airbnb_cv, 
            metrics = mset,
            grid = crossing(mtry = c(2, 3, 4), 
                            trees = seq(2000, 3600, 400)))

xg_tune %>%
  collect_metrics() %>%
  arrange(mean)

xg_fit <- xg_wf %>%
  finalize_workflow(select_best(xg_tune)) %>%
  fit(listings)

saveRDS(xg_fit, file = "xg_fit.rds")


# grid_control <- control_grid(save_pred = TRUE, 
#                              save_workflow = TRUE,
#                              extract = exract_model)

