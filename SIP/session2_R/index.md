# Advanced Tidyverse and Dashboarding

*Instructors: Ted Papalexopoulos, Andy Zheng*

In this session we will learn how to build interactive dashboards using the `shiny` package. These are useful in both in the exploration phase of an analysis and for communicating results to a broader audience. We will work through creating a bare-bones version of the AirBnB website, where we will be able to look at prices and availability of listings for an extended stay at one of Boston's finest places. 

In the first half of this session session we will deep dive into the more advanced functionalities of the `tidyverse` that can help streamline data manipulation, and will help us build the back-end (server-side) part of the application to produce
data for shiny to display. Along the way, we will learn several new functions for data manipulation that minimize code repetition, are computationally more efficient, and make downstream tasks in modeling and visualization easier. We will also cover the different ways to merge datasets (table joins). 

In the second half, we will introduce the `shiny` package. We will learn about reactive data and functions, how to map between inputs and outputs, and how to create a simple interactive dashboard to present results of an analysis. We will code up an app that displays listings and associated prices for a hypothetical stay in Boston. 

![](http://r4ds.had.co.nz/diagrams/data-science.png)

*Image credit: Hadley Wickham*

## Learning Objectives 

By the end of this session, students will be able to:

1. Use scoped versions of `dplyr` verbs to manipulate multiple variables at once, as well as window and ranking functions. 
2. Use the six different types of table joins for merging datasets, common pitfalls and 
4. Use `shiny` to create a simple interactive dashboard for exploring data and communicating insights.

## Preassignment

Run the following command in RStudio to install/update the packages we'll be using: 

`install.packages(c("shiny", "tidyverse", "lubridate", "leaflet"))`

Also please download the student script (below) and save it in your favored working directory. Make sure you can run lines 31-32 in RStudio, which read in the two AirBnB datasets we will be using. You will likely have to set your working directory in RStudio and change the paths to point to the correct files, which are the same as were used in SIP session 1. 

## Session Materials

All of the following scripts should be placed in the same directory. You may need to change the data paths in the first three to match where you've saved the `listings` and `calendar` AirBnB datasets. 

- [Student script](https://philchodrow.github.io/mban_orientation/2_SIP/3_advanced_topics/script_student.R) 
- [Full script](https://philchodrow.github.io/mban_orientation/2_SIP/3_advanced_topics/script_complete.R)
- [Shiny prep script](https://philchodrow.github.io/mban_orientation/2_SIP/3_advanced_topics/shiny_prep.R) (this is a required initialization script for the shiny app)
- [Shiny App](https://philchodrow.github.io/mban_orientation/2_SIP/3_advanced_topics/app.R)
- [Join Slides](https://philchodrow.github.io/mban_orientation/2_SIP/3_advanced_topics/joins.pdf)
