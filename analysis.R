#Package installs
install.packages('tidyverse')
install.packages('lubridate')
install.packages("hms")

#Package inits
library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)
library(scales)

#File imports and associations
apr_22_04 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202204-divvy-tripdata.csv")
may_22_05 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202205-divvy-tripdata.csv")
jun_22_06 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202206-divvy-tripdata.csv")
jul_22_07 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202207-divvy-tripdata.csv")
aug_22_08 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202208-divvy-tripdata.csv")
sep_22_09 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202209-divvy-publictripdata.csv")
oct_22_10 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202210-divvy-tripdata.csv")
nov_22_11 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202211-divvy-tripdata.csv")
dec_22_12 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202212-divvy-tripdata.csv")
jan_23_01 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202301-divvy-tripdata.csv")
feb_23_02 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202302-divvy-tripdata.csv")
mar_23_03 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202303-divvy-tripdata.csv")
apr_23_04 <- read_csv("/Users/Kevin/Desktop/Capstone/BikeshareData/202304-divvy-tripdata.csv")

#remove mathmatical notion from graphing
options(scipen = 999)

#merge all data to work with a single table
ride_data <- rbind(apr_22_04, may_22_05, jun_22_06, jul_22_07, aug_22_08, sep_22_09, oct_22_10, nov_22_11, dec_22_12, jan_23_01, feb_23_02, mar_23_03, apr_23_04)


#get the time differences between start and end destinations to see how long each rider is run. 
diff_time <- as_hms(difftime(ride_data$ended_at, ride_data$started_at))

#Take the comparison and create a data frame that allows us to compare the ride times to the membership type of the rider. 
time_compare <- data.frame(start_time =  ride_data$started_at,
                           end_time = ride_data$ended_at,
                           total_ride_time = diff_time,
                           member_type = ride_data$member_casual)


#Compare ride_types and the total for each type
ride_data %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

#Get month of ride
month <- month(time_compare$start_time, label = TRUE)
head(month)

#define plottable data
day_of_week <- wday(time_compare$start_time, label = TRUE)
mem <- ride_data$member_casual
bike <- ride_data$rideable_type


#count the bike types and their rides based on members
bike_type_count <- data.frame(bike, mem) %>%
  group_by(bike) %>%
  count(bike, mem)


#Check for day of week and count comparison.
day_of_week_mem <- data.frame(mem, day_of_week, month)
day_of_week_mem %>%
  group_by(mem, day_of_week) %>% 
  count(day_of_week)

#Check the month of usage
day_of_week_mem %>%
  group_by(mem, month) %>% 
  count(month)

#Count each member types usage of particular bike types
ride_data %>% count(rideable_type, member_casual, sort=TRUE)

#count each member type based on stations
ride_data %>% count(start_station_name, member_casual, sort=TRUE)
ride_data %>% count(end_station_name, member_casual, sort=TRUE)

#set new dataframe to host the start and end times of each ride
time_of_day <- data.frame(start_time =  hour(ride_data$started_at),
                          end_time = hour(ride_data$ended_at),
                          member_type = ride_data$member_casual)

time_of_day_compare <- time_of_day %>% 
  group_by(member_type, start_time) %>% 
  count(start_time)


#PLOTTING THE DATA TO SEE THE ANALYSIS

#plot comparison of rider types vs month of year showing in a count type
types_vs_month <- ggplot(day_of_week_mem, mapping = aes(mem, month)) + 
  geom_count(mapping = aes(x=mem, y=month, color=mem)) + 
  labs(size="Number of rides",col="Membership type", x="Membership type", y="Month")

#count the bike types and their rides based on members
bike_types_members <- ggplot(bike_type_count,mapping = aes(x = bike,y = n)) + 
  geom_bar(aes(fill = mem),stat = "identity",position = "stack")+ 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(title="Bike preference by membership", subtitle = "Cyclic data Apr 2022 - Apr 2023", y="Total number of rides",x="Bike Type", fill="Member Type")

#Builds a bar graph to show which of the two membership types ride more (based on the length of time spent from start to finish)
bike_ride_time_compare <- time_compare %>%
  ggplot(time_compare, mapping = aes(x=member_type, y=total_ride_time, fill=member_type)) +
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(title="Total rides between April 2022 and April 2023 by membership", subtitle = "Cyclic data Apr 2022 - Apr 2023", y="Total number of rides",x="Membership Type", fill="Member Type")

#Builds graph to show the time of day preferences per each member type
time_of_day_pref <- ggplot(time_of_day_compare, mapping = aes(x=start_time, y=n, color=member_type)) +
  geom_line() + 
  scale_x_continuous(labels=as.character(time_of_day_compare$start_time),breaks=time_of_day_compare$start_time) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-5)) +
  labs(title="Time of day preference by membership", subtitle = "Cyclic data Apr 2022 - Apr 2023", y="Total instance count",x="Start time (24hr time)", color="Member Type")
