# 259 Homework - integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

# ANSWER

setwd('us-weather-history')
reading_single_file <- read_csv("KCLT.csv")
read_weather <- function(s1){
  for (i in 1:length(s1)){
    df <- read_csv(paste0(s1[i], ".csv")) 
    df$date <- as.Date(df$date) #converting char date to a date format
    df$Stations_name <- s1[i]
  }
  return(df)
}

#-------------------------------------------------------------------------------
  
# QUESTION 2
#> Use map_dfr() and your new function to read in all 10 stations
#> map_dfr() will take each dataframe and automatically bind them.
#> Save the resulting dataset to "ds"

# ANSWER

ds <- map_dfr(stations,read_weather)

#-------------------------------------------------------------------------------

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

# ANSWER

ds$City <- factor(ds$Stations_name, levels = stations, labels = cities)
fct_count(ds$City)

#--------------------------------------------------------------------------------

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

# ANSWER

ftoc <- function(f){
  celsius <- (f - 32)*(5/9)
}
temp_columns <- c("actual_mean_temp","actual_min_temp","actual_max_temp","average_min_temp","average_max_temp","record_min_temp","record_max_temp")
ds <- ds %>% mutate(across(.cols = temp_columns,~ftoc(.x)))
ds <- ds %>% mutate(across(.cols = temp_columns,~round(.x,digits=1)))

#--------------------------------------------------------------------------------

### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.
#> 
# I don't know if I have to type a code to check the data, but the ds variable and the compiled_data.csv are the same

#--------------------------------------------------------------------------------

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

# ANSWER

#--------------------------------------------------------------------------------

# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

# ANSWER

ds$date <- as.Date(ds$date, format = "%Y-%m-%d")
ds$date <- format(ds$date, "%m")
as.factor(ds$date)
split_ds <- split(ds, ds$date)
split_ds

#---------------------------------------------------------------------------------

# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

# ANSWER

months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
for (i in months){
  cor1[i]$i <- cor(split_ds[[i]]$actual_precipitation, split_ds[[i]]$average_precipitation)
  cor2[i]$i <- cor(split_ds[[i]]$actual_min_temp, split_ds[[i]]$average_min_temp)
  cor3[i]$i <- cor(split_ds[[i]]$actual_max_temp, split_ds[[i]]$average_max_temp)
}
#> I got a bunch of warning messages - don't know what that means, would love to discuss this in class
#> I also added only the number of the month - I didn't know if you wanted the month's name or not, but this seemed
#> easier, so I just left it as is

#---------------------------------------------------------------------------------

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

# ANSWER

ds %>% select(City, actual_mean_temp:record_precipitation) %>% plot_boxplot(by = "City")
ds %>% select(date, actual_mean_temp:record_precipitation) %>% plot_boxplot(by = "date")
plot_correlation(ds, type = c("continuous"))

#----------------------------------------------------------------------------------

# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

# ANSWER

ds$date <- as.numeric(ds$date)
ggplot(ds, aes(x = date, y = actual_mean_temp)) + # scatter plot should have an x and a y value
  geom_point() +  # geom_point is the function for scatter plot, gives one point for each (x,y) value
  xlim(1, 12) + #sets limits of the x and y axes
  ylim(-40, 40) + 
  xlab("Month") + 
  ylab("Actual Mean Temperature") +
  geom_vline(xintercept = 0) + #adds a vertical reference line and a horizontal ref line
  geom_hline(yintercept = 0)

# Facet wrapping with 3 columns
ggplot(ds, aes(x = date, y = actual_mean_temp)) + 
  geom_point() +
  facet_wrap("City", ncol = 3)

# Plotting by month and different colors
  ggplot(ds, aes(x = date, y = actual_mean_temp, color = date)) + 
    geom_point()
  
#----------------------------------------------------------------------------------
  
# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month


