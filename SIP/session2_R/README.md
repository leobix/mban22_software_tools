# Advanced Tidyverse and Dashboarding

*Instructors: Ted Papalexopoulos, Andy Zheng, Leonard Boussioux*

In this session we will learn how to build interactive dashboards using the `shiny` package. These are useful in both in the exploration phase of an analysis and for communicating results to a broader audience. We will work through creating a bare-bones version of the AirBnB website, where we will be able to look at prices and availability of listings for an extended stay at one of Boston's finest apartments. 

In the first half of this session session we will deep dive into the more advanced functionalities of the `tidyverse` that can help streamline data manipulation, and will help us build the back-end (server-side) part of the application. Along the way, we will learn several new functions for data manipulation that minimize code repetition, are computationally more efficient, and make downstream tasks in modeling and visualization easier. We will also cover the different ways to merge datasets (table joins). 

In the second half, we will introduce the `shiny` package. We will learn about reactive data and functions, how to map between inputs and outputs, and how to create a simple interactive dashboard to present results of an analysis. We will code up an app that displays listings and associated prices for a hypothetical stay in Boston. 

![](http://r4ds.had.co.nz/diagrams/data-science.png)

*Image credit: Hadley Wickham*

## Learning Objectives 

By the end of this session, students will be able to:

- Leverage the full power of `group_by` with summarise/mutate/filter, including window and ranking functions. 
- Use scoped versions of `dplyr` verbs to manipulate multiple variables at once. 
- Understand the six different types of table joins for merging datasets.
- Create simple interactive dashboards for exploring data and communicating insights using the `shiny` package.

## Preassignment

#### 1. Install R packages

Open RStudio and run the following command to install/update the packages we'll be using: 

`install.packages(c("shiny", "tidyverse", "lubridate", "leaflet"))`

#### 2. Check Shiny installation:

Check that you can create a simple `shiny` app that runs locally:

1) In RStudio, click on `File -> New File -> Shiny Web App...`
    - *Application Name*: Give a name to your application, e.g. `test_app`. 
    - *Application Type*: Keep as `Single File (app.R)`
    - *Create within directory*: The directory the app code will be saved in. Can be anywhere you want it.
2) Click on `Create`. This should open a script called `app.R` in your RStudio. 
    - We will see what all of this code does during the session, you don't need to understand it.
3) On the top right of the script, click the `Run App` button (next to the green triangle).
    - This should open a browser window with a slider on the left and histogram on the right. 
    - Check that you can drag the slider, and that the histogram changes accordingly.
4) After this, you can close the script and delete the app directory. We won't need it during the session.

#### 3. Download session code

You saw how to use `git` during Orientation. If you didn't clone this repository then, do so now:

1) Open the Terminal (Mac) or Git Bash (Windows). 
2) Use `cd` to navigate to the directory you want to clone this code to.
3) Run `git clone https://github.com/leobix/mban22_software_tools.git`
4) This should create a directory called `mban22_software_tools` in your chosen directory.


Or if you already have it cloned from Orientation, update by pulling from the remote repo:

1) Open the Terminal (Mac) or Git Bash (Windows). 
2) Use `cd` to navigate to your local `mban22_software_tools` repo.
3) Run `git pull` to get the most recent changes into your local repo.


#### 4. Test session code

Now if you navigate to the cloned repository in Finder (Mac) or File Explorer (Windows), you should be able to find this session's materials in the directory `mban22_software_tools -> SIP -> session2_R`.

1) Unzip the `data.zip` file. 
2) Open `session2_R/script_student.R`  (this should open up in RStudio)
3) Set your working directory to `session2_R`
    - The easiest way to do this is to have the above script open and...
    - From the navigation bar `Session -> Set Working Directory -> To Source File Location`
4) Load the tidyverse by running `library(tidyverse)`
5) Make sure you read the csvs by running lines 35-36 of `script_student.R`.


That's it, see you on Thursday!