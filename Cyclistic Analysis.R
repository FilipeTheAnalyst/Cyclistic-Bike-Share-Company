library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Cyclistic 2021 datasets (csv files) here

m01_2021 <- read_csv("202101-divvy-tripdata.csv")
m02_2021 <- read_csv("202102-divvy-tripdata.csv")
m03_2021 <- read_csv("202103-divvy-tripdata.csv")
m04_2021 <- read_csv("202104-divvy-tripdata.csv")
m05_2021 <- read_csv("202105-divvy-tripdata.csv")
m06_2021 <- read_csv("202106-divvy-tripdata.csv")
m07_2021 <- read_csv("202107-divvy-tripdata.csv")
m08_2021 <- read_csv("202108-divvy-tripdata.csv")
m09_2021 <- read_csv("202109-divvy-tripdata.csv")
m10_2021 <- read_csv("202110-divvy-tripdata.csv")
m11_2021 <- read_csv("202111-divvy-tripdata.csv")
m12_2021 <- read_csv("202112-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

colnames(m01_2021)
colnames(m02_2021)
colnames(m03_2021)
colnames(m04_2021)
colnames(m05_2021)
colnames(m06_2021)
colnames(m07_2021)
colnames(m08_2021)
colnames(m09_2021)
colnames(m10_2021)
colnames(m11_2021)
colnames(m12_2021)

# Inspect the dataframes and look for incongruities
str(m01_2021)
str(m02_2021)
str(m03_2021)
str(m04_2021)
str(m05_2021)
str(m06_2021)
str(m07_2021)
str(m08_2021)
str(m09_2021)
str(m10_2021)
str(m11_2021)
str(m12_2021)

# Stack individual monthly's data frames into one big data frame
all_trips <- bind_rows(m01_2021, m02_2021, m03_2021, m04_2021,
                       m05_2021, m06_2021, m07_2021, m08_2021,
                       m09_2021, m10_2021, m11_2021, m12_2021)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

#remove the latitude, longitude, start_station_id and end_station_id as these are not required for analysis
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id))

#rename column member_casual to rider_type
all_trips <- all_trips %>%
  rename(rider_type = member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                           "Friday", "Saturday")[as.POSIXlt(all_trips$date)$wday + 1]
all_trips$hour_of_day <- format(as.POSIXct(all_trips$started_at), format = "%H")


# Add a "ride_length" calculation to all_trips (in minutes)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# sort the dataset based on ride_length value to check if there are any errors
all_trips %>%
  arrange(ride_length) %>%
  select(ride_id, started_at, ended_at, ride_length) %>%
  filter(ride_length < 0)

# number of rows in dataset
nrow(all_trips)

# Remove "bad" data
# The dataframe includes records  where ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]

#number of rows in dataset after removing records with negative ride_length
nrow(all_trips_v2)

# check for NA values in start_station_name and end_station_name columns
all_trips_v2 %>% 
  group_by(start_station_name) %>%
  summarise(number_of_rides = n()) %>%
  arrange(-number_of_rides)

all_trips_v2 %>% 
  group_by(end_station_name) %>%
  summarise(number_of_rides = n()) %>%
  arrange(-number_of_rides)

# replace NA values in start_station_name and end_station_name columns with Missing Data values
all_trips_v2$start_station_name <-
  replace(all_trips_v2$start_station_name, is.na(all_trips_v2$start_station_name), "Missing Data")
all_trips_v2$end_station_name <-
  replace(all_trips_v2$end_station_name, is.na(all_trips_v2$end_station_name), "Missing Data")

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in minutes)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$rider_type, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$rider_type, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$rider_type, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$rider_type, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$rider_type + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Monday", "Tuesday", 
                                                                       "Wednesday", "Thursday", 
                                                                       "Friday", 
                                                                       "Saturday", "Sunday"))

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  #groups by usertype and weekday
  group_by(rider_type, day_of_week) %>%
  #calculates the number of rides and average duration
  summarise(number_of_rides = n()
            # calculates the average duration
            ,average_duration = mean(ride_length)) %>%
  # sorts
  arrange(rider_type, day_of_week)



#=================================================
# STEP 5: EXPORT FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Tableau
counts <- write.csv(all_trips_v2, file = 'all_trips.csv')


