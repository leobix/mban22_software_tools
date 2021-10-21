
#setwd("~/classes/mban_orientation/2_SIP/3_advanced_topics")

library(tidyverse) #' includes dplyr, tidyr, ggplot2
library(lubridate) #' for manipulating dates and times
library(leaflet)   #' for geospatial visualization
library(ggmap)     #' for plotting on a map


#' Read data sets
listings = read_csv('../data/listings.csv')
calendar = read_csv('../data/calendar.csv')

#' Clean prices
listings = listings %>% mutate_at(vars(contains("price")), parse_number)
calendar = calendar %>% mutate_at(vars(contains("price")), parse_number)

#' Filter to best listings
creme_de_la_creme = listings %>%
    group_by(neighbourhood_cleansed) %>%
    filter_at(
        vars(starts_with("review_scores")), 
        function(x) x > mean(x, na.rm = T)) %>%
    ungroup() %>%
    select(id, name, neighbourhood_cleansed, property_type,
           accommodates, bedrooms, bathrooms, 
           latitude, longitude) # Picking out some columns

#' November/December data frames for rolling window availability
nov_weekends = calendar %>%
    filter(year(date) == 2019, 
           month(date) == 11, 
           weekdays(date) == "Friday") %>%
    select(listing_id, date, minimum_nights, maximum_nights) %>%
    semi_join(creme_de_la_creme, by = c("listing_id" = "id"))

novdec_cal = calendar %>% 
    filter(year(date) == 2019, 
           month(date) == 11 | month(date) == 12) %>%
    select(listing_id, date, available, adjusted_price) %>%
    semi_join(creme_de_la_creme, by = c("listing_id" = "id"))

# Cross-joined version to use with function
avail_long = nov_weekends %>%
    inner_join(novdec_cal, by = "listing_id") %>%
    rename(stay_start = date.x, stay_end = date.y) %>%
    mutate(diff_days = as.numeric(difftime(stay_end, stay_start, units = "days")))

# Function to get the base plotting data frame
get_availability_table = function(ndays, npeople){
    # Filter and summarise to get available listings and price per day for an ndays stay
    avail = avail_long %>%
        filter(diff_days >= 0,
               diff_days < ndays, 
               ndays >= minimum_nights, 
               ndays <= maximum_nights) %>%
        group_by(listing_id, stay_start) %>%
        summarise(total_price = sum(adjusted_price), 
                  available_all = all(available)) %>%
        ungroup() %>%
        filter(available_all) %>%
        select(listing_id, stay_start, total_price) 
    
    # Join to creme de la creme, filter to those that accomodate more than npeople
    # and calculate price per day per person
    avail = avail %>%
        inner_join(creme_de_la_creme, by = c("listing_id" = "id")) %>% 
        filter(accommodates > npeople) %>% 
        mutate(price_per_day_person = total_price / (ndays * npeople))
        
    return(avail)
}


ndays = 4
npeople = 4
weekend_of = ymd(20191108)
ndays = 4
npeople = 4

toplot = get_availability_table(ndays, npeople) %>%
    filter(stay_start == weekend_of)

    
toplot %>% 
    gather(type, number, accommodates, bedrooms, bathrooms) %>%
    ggplot(aes(x=number)) +
    geom_bar() + 
    facet_grid(~type, scales = "free") + 
    labs(x = "#", y = "number of listings")

toplot %>% 
    ggplot(aes(x = price_per_day_person)) + 
    geom_histogram()

boston_coords <- c(left   = -71.1289, 
                   bottom = 42.3201, 
                   right  = -71.0189, 
                   top    = 42.3701)

basemap <- get_map(location = boston_coords,
                   maptype = 'terrain')

ggmap(basemap) + 
    geom_point(aes(x = longitude, y = latitude, col = price_per_day_person), 
               data = toplot, 
               size = 3)

