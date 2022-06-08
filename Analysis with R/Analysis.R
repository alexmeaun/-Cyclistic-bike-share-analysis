install.packages("tidyverse")
install.packages("lubricate")
install.packages("ggplot2")

# Importing tidyverse for wrangle data
library(tidyverse)
# Importing lubridate for wrangle data attributes
library(lubridate)
# Importing ggplot2 for visualizing data
library(ggplot2)

getwd()
setwd("C:/Users/alexm/Desktop/Stuff/Certificates/Google Data Analytics/Data Analytics projects/Cyclistic/CSV")
getwd()



## Step 1: Collect data
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")



## Step 2: Wrangle data and combine in a single file
# Task 1: Compare column names each of the files
colnames(q1_2020)
colnames(q4_2019)
colnames(q3_2019)
colnames(q2_2019)

# Rename columns  to make them consistent with q1_2020
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "X01...Rental.Details.Rental.ID"
                   ,rideable_type = "X01...Rental.Details.Bike.ID" 
                   ,started_at = "X01...Rental.Details.Local.Start.Time"  
                   ,ended_at = "X01...Rental.Details.Local.End.Time"  
                   ,start_station_name = "X03...Rental.Start.Station.Name" 
                   ,start_station_id = "X03...Rental.Start.Station.ID"
                   ,end_station_name = "X02...Rental.End.Station.Name" 
                   ,end_station_id = "X02...Rental.End.Station.ID"
                   ,member_casual = "User.Type"))


# Inspect the dataframes and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)


# Convert ride_id and rideable_type to character so that they can stack correctly
mutate_file <- function(temp_file){
  temp_file <- mutate(temp_file, ride_id=as.character(ride_id), 
                      rideable_type = as.character(rideable_type))
  return(temp_file)
}
q4_2019  <- mutate_file(q4_2019 )
q3_2019  <- mutate_file(q3_2019 )
q2_2019  <- mutate_file(q2_2019 )


# Stack individual year's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

colnames(all_trips)          


# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, 
            "X01...Rental.Details.Duration.In.Seconds.Uncapped", 
            "X05...Member.Details.Member.Birthday.Year", "Member.Gender", 
            "tripduration"))



## Step 3: Clean up and prepare data for analysis
# Inspect the new table

#List of column names
colnames(all_trips) 
#How many rows are in data frame
nrow(all_trips) 
#Dimensions of the data frame.
dim(all_trips)
#See the first 6 rows of data frame.
head(all_trips) 
#See list of columns and data types.
str(all_trips)  
#Statistical summary of data.
summary(all_trips)  


# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" 
#and "Subscriber") and two names for casual riders ("Customer" and "casual"). 
#We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. 
#We will want to add some additional columns of data -- such as day, month, 
#year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 
#2020Q1 data did not have the "tripduration" column. We will add "ride_length" 
#to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including 
#several hundred rides where Divvy took bikes out of circulation for Quality 
#Control reasons. We will want to delete these rides.

#(1) # In the "member_casual" column, I'll replace "Subscriber" with "member" 
#and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders. I 
#will want to make the dataframe consistent with their current nomenclature
# Checking how many observations fall under each usertype
table(all_trips$member_casual)

# Reassign to the desired values
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow to aggregate ride data for each month, day, or year. 
# Before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Adding a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so I'll be able to run 
#calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of 
#docks and checked for quality by Divvy or ride_length was negative
# I'll create a new version of the dataframe
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | 
                              all_trips$ride_length<0),]


# Step 4: Descreptive analysis
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride


summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

# Fixing the problem with the days of the week that are out of order.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", 
                                             "Wednesday", "Thursday", "Friday", 
                                             "Saturday"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	# sorts

# Visualizing the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


# Export sumamry file for further analysis
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

                    