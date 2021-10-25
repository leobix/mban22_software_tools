
#setwd("~/classes/mban_orientation/2_SIP/3_advanced_topics")

library(tidyverse) #' includes dplyr, tidyr, ggplot2
library(lubridate) #' for manipulating dates and times
library(leaflet)   #' for geospatial visualization
library(ggmap)     #' for plotting on a map


#' Read data sets
listings = read_csv('data/listings.csv')
calendar = read_csv('data/calendar.csv')

#' Clean prices
listings = listings %>% 
    mutate(price  = parse_number(weekly_price)) %>% 
    rename(neighb = neighbourhood_cleansed) %>% 
    select(-starts_with("host_")) %>% 
    select(id, name, neighb, 
           ends_with("price"), 
           starts_with("review_scores"), 
           everything())

creme_de_la_creme = listings %>%
    group_by(neighb) %>%
    filter_at(vars(starts_with("review_scores")), 
              function(x) x > mean(x, na.rm = T)) %>%
    ungroup()

cdc_cal = calendar %>% 
    semi_join(creme_de_la_creme, by = c("listing_id" = "id")) %>% 
    mutate(price = parse_number(adjusted_price)) %>% 
    select(-adjusted_price)



get_availability_table = function(start_of_stay, n_days, n_people){
    # Filter if a listing fits our stay:
    fits_stay = cdc_cal %>% 
        filter(date == start_of_stay) %>% 
        filter(n_days >= minimum_nights, 
               n_days <= maximum_nights)
    
    # Filter candidates:
    candidates = creme_de_la_creme %>%
        filter(accommodates >= n_people) %>% 
        semi_join(fits_stay, by = c("id" = "listing_id"))
    
    # Compute availability and daily rate for candidates:
    prices = cdc_cal %>% 
        semi_join(candidates, by = c("listing_id" = "id")) %>% 
        filter(date >= start_of_stay, 
               date <= start_of_stay + days(n_days - 1)) %>% 
        group_by(listing_id) %>% 
        summarise(available_n = all(available), 
                  price_n = sum(price)) %>% 
        filter(available_n) %>% 
        transmute(id = listing_id, 
                  price_per_day_person = round(price_n / (n_days * n_people)))
    
    avail = creme_de_la_creme %>% 
        inner_join(prices, by = "id") %>% 
        transmute(id, name, neighb, 
                  price_per_day_person, accommodates, 
                  longitude, latitude,
                  rating = review_scores_rating)
    return(avail)
}
