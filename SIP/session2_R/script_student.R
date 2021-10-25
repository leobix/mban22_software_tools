#' -------------------------------------------------------------------------
#' *Advanced Tidyverse*
#' Instructors: Ted Papalexopoulos, Andy Zheng, Leonard Boussioux
#' MBAn Software Tools - Fall 2021
#' -------------------------------------------------------------------------
#' In this session we will work towards building a bare-bones version
#' of the AirBnB website, where we can explore prices and map listings 
#' for a potential stay. 
#' 
#' This script will help us build the *server*-side functions that 
#' manipulate data for shiny to use. As we build them up we will look 
#' at advanced functionalities of dplyr and data merging. 


#' -------------------------------------------------------------------------
#' Load data and libraries
#' -------------------------------------------------------------------------
#' Prior to running any code, you probably want to:
#' `Session > Set Working Directory > To Source File Location`
#' 
#' We'll be working with relatively large datafiles, which you will
#' find in *data.zip* in this directory. Unzip them and make sure
#' the relative paths work for `read_csv()` below. 

library(tidyverse) #' includes dplyr, tidyr, ggplot2
library(lubridate) #' for manipulating dates and times

#' For the analysis we are going to do today, we need two data sets: 
#' - The `listings` data set contains detailed information about each 
#'   AirBnB listing. We particularly care about:
#'       * property type
#' - The `calendar` data set contains, for each listing, the availability
#'   and price on each day over the next year. 

listings = read_csv('data/listings_clean.csv')
calendar = read_csv('data/calendar.csv')

#' Let's take a look at each of these:
View(listings)
View(calendar)

#' -------------------------------------------------------------------------
#' RECAP & VARIABLE SELECTORS
#' -------------------------------------------------------------------------
#' Let's recall the main verbs of dplyr:
#'   `select()` to select variables based on their names.
#'   `filter()` to select rows based on their values.
#'   `arrange()` to reorder rows.
#'   `mutate()` to add new variables that are functions of existing variables.
#'   `summarise()` to condense multiple values to a single value.
#' 
#' What makes these so powerful is the *chaining operator* `%>%`. Every
#' verb takes a data.frame as input and gives a data.frame as output. 
#' So we can sequentially program our steps.


#' Let's start simple. `select()` picks certain columns. 
listings %>% 
    select(id, review_scores_rating) %>% 
    head


#' We can also remove columns by adding a `-` in `select()`:
listings %>% 
    select(-name, -host_name, -host_since, 
           -host_response_time, -host_response_rate) %>% 
    head

#' The `tidyverse` offers a bunch of convenience *selector* functions
#' to help you be lazy when selecting columns. This will also be 
#' useful once we get to scoped verbs.

#' Suppose I want to look at all columns that are named
#' `review_scores_<something>`. I could type them all out...
listings %>% select(
    id, 
    review_scores_rating,
    review_scores_accuracy, 
    review_scores_cleanliness, 
    review_scores_checkin, 
    review_scores_communication, 
    review_scores_location, 
    review_scores_value
)

#' Or I can use variable selectors to select columns based on their name:
#'    `starts_with`(string)
#'    `ends_with`(string)
#'    `contains`(string)
#'    `matches`(regex)
#'    `everything`()
#'    `last_col`(offset)

#' So with much less typing, I can do the same as above...
listings %>% select(id, starts_with("review_scores"))


#' The `-` also works:
listings %>% select(-starts_with("review_scores"))


#' Often you want to rearrange the order of columns for easier viewing.
#' You can combine selectors in any way: 
listings %>% 
    select(id, 
           neighbourhood_cleansed, 
           starts_with("host_"), 
           everything())


#' Let's move on to `mutate()`, which adds new columns. Let's
#' create a `rating` as a percentage:
listings %>% 
    select(id, review_scores_rating) %>% 
    mutate(rating = review_scores_rating / 100) %>% 
    head


#' Bonus verb time! `transmute()` verb works like a combination of 
#' `select()` and `mutate()`:
listings %>% 
    transmute(id, 
              review_scores_rating,
              rating = review_scores_rating / 100) %>% 
    head


#' Use `filter()` to select *Downtown* neighbourhood AND
#' rating is `> 80`:
listings %>% 
    filter(neighbourhood_cleansed == "Downtown", 
           review_scores_rating > 80) %>%
    head

#' Different arguments to `filter()` are an *intersection* (ALL
#' things must be TRUE to keep the row). If we want a union, we 
#' need the `|` (or) operator:
listings %>% 
    filter(neighbourhood_cleansed == "Downtown" | 
           neighbourhood_cleansed == "Back Bay") %>% 
    head


#' The `%in%` operator is useful for this:
c("Paul", "Ravi", "John") %in% c("John", "Paul", "Ringo", "George")


#' So we could do the same as before with:
listings %>% 
    filter(neighbourhood_cleansed %in% c("Downtown", "Back Bay")) %>% 
    head


#' Now I want to point out a *very* common source of bugs. 
#' `filter()` removes rows where the T/F evaluates to *NA*
listings %>% select(id, weekly_price) %>% head
listings %>% select(id, weekly_price) %>% head %>% 
    filter(startsWith(weekly_price, "$")) 

#' Note: `startsWith()` is for checking if a string starts with a 
#' character, it's different than the `starts_with()` selector
#' we saw earlier.


#' Use `arrange()` to sort rows by rating, breaking ties by accuracy:
listings %>% 
    select(id, review_scores_rating, review_scores_accuracy) %>% 
    arrange(review_scores_rating, review_scores_accuracy) %>% 
    head


#' If we want the reverse (descending) order, we can wrap the column
#' name in `desc()`. First let's see what `desc()` does to a vector:
desc(c(4, 7, 1, 3))


#' So we can use `desc()` with any of the columns in `arrange()`:
listings %>% 
    select(id, review_scores_rating, review_scores_accuracy) %>% 
    arrange(review_scores_rating, desc(review_scores_accuracy)) %>% 
    head


#' `summarise()` will compute aggregates over the entire data frame. 
#' The aggregation should return *one* value for the entire data frame.
listings %>% 
    summarise(num_rows = n(), 
              min_rating = min(review_scores_rating, na.rm = T),
              mean_rating = mean(review_scores_rating, na.rm = T), 
              max_rating = max(review_scores_rating, na.rm = T))


#' Note the importance of `na.rm = T`: In R, aggregating functions
#' (like min, mean, max) will by default return NA if any of the elements
#' is NA.


#' -------------------------------------------------------------------------
#' EXERCISE 1: Warm-up
#' -------------------------------------------------------------------------
#' Let's do a basic data cleaning exercise. Process  a version
#' of listings with:
#'    1) Convert *weekly_price* to a number and rename it to *price*
#'       Hint: What does `parse_number()` do?
#'    2) Rename *neigbourhood_cleansed* to *neighb*.
#'       Hint: What does `rename()` do?
#'    3) Remove all of the columns that have to do with *host_...*
#'    3) Order of columns should be: 
#'          - id, name, neighb
#'          - the two price columns
#'          - all the review_scores columns
#'          - everything else
#' Reminder: typing `?<function>` into the console, brings up 
#' documentation including usage examples (at the very end).

# YOUR CODE HERE
listings = listings %>% 
    ...


#' -------------------------------------------------------------------------
#' END EXERCISE 1
#' -------------------------------------------------------------------------


#' -------------------------------------------------------------------------
#' GROUP BY
#' -------------------------------------------------------------------------
#' The `group_by(...)` verb creates groups of observations.
#' By itself, it does not doing anything to the data frame. 
#' It just adds *meta-information*. Then other verbs will 
#' adjust their behaviour accordingly. 

listings %>% dim
listings %>% group_by(neighb) %>% dim


#' `group_by() %>% select()` is the same, but also keeps
#'  the grouping variables...
listings %>% 
    group_by(neighb) %>% 
    select(price)


#' `group_by() %>% summarise()` will perform aggregation
#' within each group:
listings %>% 
    group_by(neighb) %>% 
    summarise(n = n(), 
              mean_price = mean(price, na.rm = T)) 
    

#' You can specify multiple grouping columns:
listings %>% 
    group_by(neighb, room_type) %>% 
    summarise(n = n())


#' Bonus verb! `count` is short-hand for the above:
listings %>% 
    count(neighb, room_type)



#' `group_by() %>% arrange()` will be the same, UNLESS `.by_group=TRUE`
#' in which case rows are sorted *first* by the grouping columns
#' and only then by what you asked for
listings %>% 
    select(id, neighb, price) %>%  
    group_by(neighb) %>%
    arrange(desc(price), .by_group = T) 



#' `group_by() %>% mutate()` and `group_by() %>% filter()` are the 
#' most powerful, and what we'll focus on the most.
#' 
#' If you get this, you get everything: with `mutate` and `filter`, 
#' any operation/function on a column is done *separately* for each 
#' group. Let me illustrate...

#' Let's make a function to compute z-scores of a vector of numbers. 
z_score = function(x) {
    z = (x - mean(x, na.rm=T)) / sd(x, na.rm=T)
    return(z)
}
z_score(c(2, 3, 4))


#' Recall that z-scores are *how many sds above the mean* something
#' is. It's therefore a measure of how much of an outlier a value is.
#' What if I want the z-score of the price within it's neighbourhood?
listings %>% 
    mutate(z_global = z_score(price)) %>% 
    group_by(neighb) %>% 
    mutate(z_neighb = z_score(price)) %>% 
    select(id, z_global, z_neighb) %>% 
    head


#' Note that the `mutate()` calls were exactly the same! 
#' But in the first case, the input wasn't grouped. So z_score
#' was called *once*, with the full column as input, and so 
#' gave us the z-score based on *all* listings' mean and sd.
#' 
#' In the second case, we had grouped by neighbourhood. So 
#' z_scores was called for each group *separately*. In each call,
#' the function saw the subset of the data for that neighbourhood, 
#' and so gave us the z-score based on that neighbourhood's listings.



#' Now here's a cute trick. Suppose I want to add a column to
#' *listings*, with the average price in its neighbourhood. I 
#' could summarise the average price and then merge:
listings %>% 
    group_by(neighb) %>% 
    summarise(mean_price = mean(price, na.rm=T))


#' But I could do the same in one step with `group_by() %>% mutate()`!
#' If the RHS of mutate returns *one* value (e.g. the mean), it will 
#' be *recycled* for all elements.
listings %>% 
    group_by(neighb) %>% 
    transmute(id, 
              price, 
              mean_price = mean(price, na.rm=T)) %>% 
    head


#' The same grouping logic works for `filter`. Each group
#' is filtered separately. 
listings %>% 
    group_by(neighb) %>%
    filter(price > mean(price)) %>% 
    transmute(id, price)

#' OH NO WHERE DID MY ROWS GO???? 



#' I got you. Remember how `mean(vector)` horribly returns NA in R if 
#' any element is NA? And how filter removes any rows where the T/F was 
#' NA? Well, we forgot to add *na.rm=T* to our mean call...
listings %>% 
    group_by(neighb) %>%
    filter(price > mean(price, na.rm=T)) %>% 
    transmute(id, price)

 

#' Here's another use-case for grouped mutate/filter: superlatives!
#' The `min_rank()` function finds the ranking of each element in a vector:
min_rank(c(7, 5, 7, 42))


#' So let's use it to find the cheapest listing in each neighbourhood:
listings %>% 
    group_by(neighb) %>% 
    filter(min_rank(price) == 1) %>% 
    select(id, price, review_scores_rating) %>% 
    head


#' Mini-exercise: there is no `max_rank()` function.
#' We've already seen a function today that can easily
#' get us the *most* expensive listing. You only need 
#' six extra characters in the line above...


#' There are however other useful ranking functions.
#'     `row_number()` which row is this in the group
#'     `min_rank(column)` the rank of the observation 
#'     `percent_rank(column)` the percentile rank 
#'     `ntile(column, n)` a rough rank, which tells you which of *n* bins something is in


#' -------------------------------------------------------------------------
#' EXERCISE 2: Help, `min_rank()` can't break ties!
#' -------------------------------------------------------------------------
#' As you may have noticed. There are many cheapest listings 
#' in Roslindale! Suppose I wanted break ties by rating. 
#' Unfortunately, `min_rank(v1, v2)` doesn't work:
listings %>% 
    group_by(neighb) %>% 
    filter(min_rank(price, review_scores_rating) == 1) 


#' Can you think of some way of using `row_number()` to get the cheapest
#' listing in a neighbourhood, that has the highest *review_scores_rating*?
listings %>% 
    ...


#' -------------------------------------------------------------------------
#' END EXERCISE 2
#' -------------------------------------------------------------------------


#' -------------------------------------------------------------------------
#' WARNING
#' -------------------------------------------------------------------------
#' When you `group_by() %>% mutate/filter`, any function you use will
#' be called once for every group. This creates a lot of overhead. 
#' 
#' The problem is that groups persist. If you do some grouped operation, 
#' then any future `mutate/filter` calls will feel the need to work 
#' on each group. And if they don't need to be grouped, this will make
#' your code slower. 
#' 
#' Therefore it is prudent to always `ungroup()` after you're done.  
#' Consider the following toy example which is especially bad because 
#' date manipulation is computationally expensive: 

d1 = calendar %>% 
    group_by(listing_id) %>%
    # ... do stuff ... %>%
    filter(between(date, ymd("2019-11-01"), ymd("2019-11-03")))

d2 = calendar %>% 
    group_by(listing_id) %>%
    # ... do stuff ... %>%
    ungroup %>% 
    filter(between(date, ymd("2019-11-01"), ymd("2019-11-03")))

#' Both commands create the same exact data frame, because the filter
#' result does not change when you `group_by(listing_id)`. However
#'  `d1` took ~10 seconds to run, while `d2` was instantaneous.
#'   
#' This is because the former needed to call `between` for each of
#' the 6264 listings while the latter used one big call. 



#' -------------------------------------------------------------------------
#' WORKING WITH LUBRIDATE
#' -------------------------------------------------------------------------
#' The `lubridate` package is very useful for working with
#' dates, which we'll need for working with the calendar.


#' There's very flexible functions to convert strings to dates:
dates = c("2019-02-23", "21-12-2")
dates = ymd(dates)
class(dates)
#' You can check other versions like `myd()`, `dmy_hms()` etc.


#' Once something is converted to a date, you can easily extract
#' different information:
day(dates)
week(dates)
month(dates)
year(dates)


#' Or do math with the dates:
dates + days(3)
dates + months(2)


#' One of my personal favorites is `floor_date()`\`ceiling_date()`:
floor_date(dates, unit="years")
ceiling_date(dates, unit="months")


#' There's also nice formatting function to covert back to strings:
format(dates, "%d %B %Y")
?strptime  # documentation of formatting symbols


#' While we're here, the `case_when()` functions is really useful
#' for encoding *vectorized* if/elseif/else logic. 
#' 
#' case_when(predicate1 ~ value1, 
#'           predicate2 ~ value2, 
#'           ...
#'           predicateN ~ valueN)
#' Predicates should return T/F. The output will be a vector
#' where the *i*th element is value<j>, if predicate<j> was the
#' first TRUE predicate for the *i*'th element. In other words, 
#' vectorized else-if.
#' 
#' For example, let's try to recreate the `days_in_month()` function:
case_when(month(dates) == 2                ~ 28, 
          month(dates) %in% c(4, 6, 9, 11) ~ 30, 
          TRUE                             ~ 31)
days_in_month(dates)


#' Or if we want to build a nice formatting function with date suffixes:
nice_format = function(dts) {
    day_of_month = day(dts)
    suffix = case_when(
        day_of_month %in% c(11, 12, 13) ~ "th", 
        day_of_month %% 10 == 1         ~ "st", 
        day_of_month %% 10 == 2         ~ "nd", 
        day_of_month %% 10 == 3         ~ "rd", 
        TRUE                            ~ "th"
    )
    return(paste0(day_of_month, suffix))
}
nice_format(dates)


#' -------------------------------------------------------------------------
#' SCOPED VERBS: WORKING WITH MULTIPLE COLUMNS
#' -------------------------------------------------------------------------
#' Some of the most useful features of dplyr are "scoped" 
#' versions of all the verbs:
#'     `<verb>_all`(function):            apply function to all columns
#'     `<verb>_at`(variables, function):  apply function to variables selected as above. 
#'     `<verb>_if`(predicate, function):  apply to all columns that pass some test


#' Let's start simple, let's find the mean of all `review_scores_xxx` 
#' columns by neighborhood:
listings %>%
    group_by(neighb) %>%
    select(starts_with("review_scores")) %>%      
    summarise_all(function(x) mean(x, na.rm = T))


#' Note that in all of the above cases, the function you want
#' to apply must take only *one* argument. You can pass additional 
#' arguments:
listings %>%
    group_by(neighb) %>%
    select(starts_with("review_")) %>%      
    summarise_all(mean, na.rm = T)


#' We can accomplish the same thing by using `summarise_at` without
#' having to select out the relevant variables beforehand. Note that
#' for `_at`() functions you need to wrap variable selectors in a `vars(...)`:
listings %>%
    group_by(neighb) %>% 
    summarise_at(vars(starts_with("review_")), 
                 mean, na.rm = T)


#' How about computing the percentage of `NA`'s in each review
#' score column?
listings %>% 
    group_by(neighb) %>% 
    summarise_at(vars(starts_with("review_")), 
                 function(x) mean(is.na(x)))
    
    


#' Let's now use `mutate_at()` to create neighbourhood specific 
#' z-scores for *all* review score columns:
listings %>% 
    group_by(neighb) %>% 
    mutate_at(vars(starts_with("review_")), z_score) %>% 
    select(id, starts_with("review_"))



#' Another common use case is to apply a function to every column 
#' if it meets some condition. `_if()` verbs are ideal for this. 
#' 
#' Here the first argument is a *function* that is applied to the 
#' column vector, and returns T/F if it is a column you want to 
#' do whatever with. 


# Let's select all columns where more than 50% of entries
# are NA.
listings %>% 
    select_if(function(x) (mean(is.na(x)) > 0.5))



#' Probably the most common use case here is applying to 
#' all columns of a specific type:
listings %>% 
    mutate_if(is.Date, function(x) format(x, "%d %B %Y")) %>%
    select(id, first_review, last_review)


#' -------------------------------------------------------------------------
#' EXERCISE 3: Let's boogie like I'm bougie.
#' -------------------------------------------------------------------------
#' I'm celebrating my thesis defense with some friends
#' and I want to book an AirBnB that is above average 
#' in ALL *review_score* columns. 
#' 
#' Use `filter_at()` to do this. The first argument
#' should be the set of columns you want to filter on. 
#' The second argument should be a function that takes
#' one of those columns, and returns a T/F vector of whether
#' each element is satisfactory (i.e. is the listing above 
#' average for that column?). `filter_at()` will then AND
#' all of the T/F to keep the ones we want. 
#'
#' Hint: The result should have 1389 rows. 

creme_de_la_creme = listings %>%
    ...



#' -------------------------------------------------------------------------
#' END EXERCISE 3
#' -------------------------------------------------------------------------

#' I've found my acceptable options, now I want to look 
#' at the calendar to check their availability. 
#' Let's go to the slides for a little lesson on joining 
#' datasets. 




#' -------------------------------------------------------------------------
#' JOINING
#' -------------------------------------------------------------------------
#' To finish up our server side functions, we need 
#' a way of checking the *calendar* to see if a listing
#' is available for the days and number of people we want, 
#' and what the daily rate is.

start_of_stay = ymd("2019-11-01")
n_days = 3
n_people = 4


#' Here's what we need to do: 
#'    1) Check that a listing *accomodates* enough people
#'    2) Check that our stay is within *minimum_stay* and *maximum_stay*
#'       requirements for our days.
#'    3) Check if listing is *available* for all of the days.
#'    4) Compute the total *adjusted_price*.
#' We will do this step by step...


#' First of all, let's slightly clean up the `calendar` data frame:
#'   * We will be using the `adjusted_price` (after discounts), so 
#'     let's convert that to a number. 
#'   * Since the data frame is huge, and we will only ever
#'     consider `creme_de_la_creme` listings, let's save some 
#'     computation by *semi_joining* it them...

cdc_cal = calendar %>% 
    semi_join(creme_de_la_creme, by = c("listing_id" = "id")) %>% 
    mutate(price = parse_number(adjusted_price)) %>% 
    select(-adjusted_price)



#' 1) Let's first start with all `creme_de_la_creme` listings as
#'    our candidates, and filter out the ones that don't accommodate 
#'    enough people:
candidates = creme_de_la_creme %>% 
    filter(accommodates >= n_people)


#' 2) Now let's filter the *calendar* to those days where
#'    our fit stays. 
fits_stay = cdc_cal %>% 
    filter(date == start_of_stay) %>% 
    filter(n_days >= minimum_nights, 
           n_days <= maximum_nights)

candidates = candidates %>% 
    semi_join(fits_stay, by = c("id" = "listing_id"))



#' Now comes the interesting part. We want to compute 
#' availability and price of each candidate over the period
#' we're interested. Let's first look at a dumb way to do this, 
#' mostly because we I want to introduce `lag()`/`lead()`.
#' See if you can see what the following does...
lag(1:7)
lead(1:7, n=2)



#' One way we could get whether each listing is available
#' is the following:
cdc_cal %>% 
    semi_join(candidates, by = c("listing_id" = "id")) %>% 
    group_by(listing_id) %>% 
    mutate(available_3 = available & lead(available, 1) & lead(available, 2), 
           price_3     = price     + lead(price, 1)     + lead(price, 2)    )


#' This way you can get availability & price of 3-day stay
#' on any day! But of course, we had to hard-code 3 `lead`s, 
#' so what if we wanted fewer or more days?


#' Mini-exercise: why was `group_by(listing_id)` necessary here?
#' Also did we assume anything about the rows of *cdc_cal*?



#' -------------------------------------------------------------------------
#' EXERCISE 4: Summarise, summarise, summarise.
#' -------------------------------------------------------------------------
#' Our goal is to create a dataframe that:
#'    *  Has one row per listing in `candidates`
#'    *  Column `available_n` is TRUE if listing was available
#'       on *all* days between *start_of_stay* and 
#'       *start_of_stay + days(n_days)*, 
#'    *  Column `price_n` has the sum of prices for those days
#' 
#' Fill in the following code to achieve this:

availability = cdc_cal %>% 
    semi_join(candidates, by = c("listing_id" = "id")) %>% 
    filter(...) %>% 
    group_by(listing_id) %>% 
    summarise(available_n = ..., 
              price_n = ...)

#' Hint:
all(c(TRUE, TRUE, FALSE))
all(c(TRUE, TRUE))

#' -------------------------------------------------------------------------
#' END EXERCISE 4
#' -------------------------------------------------------------------------

#' Now we can filter to the available ones, and compute the
#' daily rate. 
prices = availability %>% 
    filter(available_n) %>% 
    transmute(id = listing_id, 
              price_per_day_person = price_n / (n_days * n_people))


final_options = creme_de_la_creme %>% 
    inner_join(prices, by = "id")



#' That's all! Now for any stay length, number of people and 
#' start date, we can find the listings that match. Let's put all 
#' of this into a function:


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
                  price_per_day_person = price_n / (n_days * n_people))
        
    avail = creme_de_la_creme %>% 
        inner_join(prices, by = "id") %>% 
        transmute(id, name, neighb, 
                  price_per_day_person, accommodates, 
                  longitude, latitude,
                  rating = review_scores_rating)
    return(avail)
}

# Let's check that this works:
avail = get_availability_table(ymd("20191101"), 3, 5) 

# And with that, we have everything we need to build our bare-bones AirBnB app. 