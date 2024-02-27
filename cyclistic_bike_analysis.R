# Install R environment
install.packages('tidyverse')
library(tidyverse)

install.packages('lubridate')
library(lubridate)

install.packages('janitor')
library(janitor)

install.packages('dplyr')
library(dplyr)

install.packages('Rcpp')
library(Rcpp)

install.packages("ggplot2")
library(ggplot2)

# Load .csv files
Feb_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202302-divvy-tripdata.csv")
Mar_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202303-divvy-tripdata.csv")
Apr_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202304-divvy-tripdata.csv")
May_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202305-divvy-tripdata.csv")
Jun_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202306-divvy-tripdata.csv")
Jul_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202307-divvy-tripdata.csv")
Aug_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202308-divvy-tripdata.csv")
Sep_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202309-divvy-tripdata.csv")
Oct_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202310-divvy-tripdata.csv")
Nov_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202311-divvy-tripdata.csv")
Dec_2023 <- read.csv("Desktop/Cyclistic Bike Analysis/202312-divvy-tripdata.csv")
Jan_2024 <- read.csv("Desktop/Cyclistic Bike Analysis/202401-divvy-tripdata.csv")

# View and check over data and structure
View(Feb_2023)
View(Mar_2023)
View(Apr_2023)
View(May_2023)
View(Jun_2023)
View(Jul_2023)
View(Aug_2023)
View(Sep_2023)
View(Oct_2023)
View(Nov_2023)
View(Dec_2023)
View(Jan_2024)

str(Feb_2023)
str(Mar_2023)
str(Apr_2023)
str(May_2023)
str(Jun_2023)
str(Jul_2023)
str(Aug_2023)
str(Sep_2023)
str(Oct_2023)
str(Nov_2023)
str(Dec_2023)
str(Jan_2024)

# Combine all 12 months into 1 dataframe
cyclistic_total <- rbind(Feb_2023,Mar_2023,Apr_2023,May_2023,Jun_2023,Jul_2023,Aug_2023,Sep_2023,Oct_2023,Nov_2023,Dec_2023,Jan_2024)

# View and check this new dataframe
View(cyclistic_total)
str(cyclistic_total)

# Remove monthly dataframes to clean up environment
remove(Apr_2023, Aug_2023, Dec_2023, Feb_2023, Jan_2024, Jul_2023, Jun_2023, Mar_2023, May_2023, Nov_2023, Oct_2023, Sep_2023)

# Make a copy of dataframe to work with
cyclistic_total_2 <- cyclistic_total

# Remove data not used for analysis
cyclistic_total_2 <- cyclistic_total_2 %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng))

# Add columns for date, month, day, year, and dat of week
cyclistic_total_2$date <- as.Date(cyclistic_total_2$started_at)
cyclistic_total_2$month <- format(as.Date(cyclistic_total_2$date), "%m")
cyclistic_total_2$day <- format(as.Date(cyclistic_total_2$date), "%d")
cyclistic_total_2$year <- format(as.Date(cyclistic_total_2$date), "%Y")
cyclistic_total_2$day_of_week <- format(as.Date(cyclistic_total_2$date), "%A")

# Add a column for ride length
cyclistic_total_2$ride_length <- difftime(cyclistic_total_2$ended_at, cyclistic_total_2$started_at)

# Convert the ride length data to numeric
is.factor(cyclistic_total_2$ride_length)
cyclistic_total_2$ride_length <- as.numeric(as.character(cyclistic_total_2$ride_length))
is.numeric(cyclistic_total_2$ride_length)

# Check summary of dataframe
summary(cyclistic_total_2)

# Remove data for rides with times less than 1 second
cyclistic_total_clean <- cyclistic_total_2[!(cyclistic_total_2$ride_length <= 0),]

# Descriptive analysis of ride_length
summary(cyclistic_total_clean$ride_length)

# Compare descriptive analysis between members and casual users
aggregate(cyclistic_total_clean$ride_length~cyclistic_total_clean$member_casual, FUN = mean)
aggregate(cyclistic_total_clean$ride_length~cyclistic_total_clean$member_casual, FUN = median)
aggregate(cyclistic_total_clean$ride_length~cyclistic_total_clean$member_casual, FUN = max)
aggregate(cyclistic_total_clean$ride_length~cyclistic_total_clean$member_casual, FUN = min)

# Compare average ride time for members and casual users by day of week
aggregate(cyclistic_total_clean$ride_length~cyclistic_total_clean$member_casual+cyclistic_total_clean$day_of_week, FUN = mean)

# Reorder days of the week
cyclistic_total_clean$day_of_week <- ordered(cyclistic_total_clean$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Rerun the average ride time by day of week
aggregate(cyclistic_total_clean$ride_length~cyclistic_total_clean$member_casual+cyclistic_total_clean$day_of_week, FUN = mean)

# Analyze ridership data by type and weekday
cyclistic_total_clean %>% 
  mutate(weekday=wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday)

# Visualize number of rides per day by rider type
cyclistic_total_clean %>% 
  mutate(weekday=wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual))+
  geom_col(position = "dodge")

# Remove scientific notation in Y axis and add labels
options(scipen = 999)
cyclistic_total_clean %>% 
  mutate(weekday=wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual))+
  geom_col(position = "dodge") +
  labs(title = "Number of Rides per Day", subtitle = "Feb 2023 - Jan 2024", fill = "Rider Type")+
  xlab("Weekday")+
  ylab("Number of Rides")

# Visualize average duration per day by rider type
cyclistic_total_clean %>% 
  mutate(weekday=wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual))+
  geom_col(position = "dodge")+
  labs(title = "Average Duration per Day", subtitle = "Feb 2023 - Jan 2024", fill = "Rider Type")+
  xlab("Month")+
  ylab("Average Duration")

# Visualize number of rides per month by rider type
cyclistic_total_clean %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x=month,y=number_of_rides,fill=member_casual))+
  geom_col(position = "dodge")+
  labs(title = "Number of Rides per Month", subtitle = "Feb 2023 - Jan 2024", fill = "Rider Type")+
  xlab("Month")+
  ylab("Number of Rides")

# Visualize average duration per month by rider type
cyclistic_total_clean %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x=month,y=average_duration,fill=member_casual))+
  geom_col(position = "dodge")+
  labs(title = "Average Duration per Month", subtitle = "Feb 2023 - Jan 2024", fill = "Rider Type")+
  xlab("Month")+
  ylab("Average Duration")

# Export summary file
cyclistic_summary <- aggregate(cyclistic_total_clean$ride_length~cyclistic_total_clean$member_casual+cyclistic_total_clean$day_of_week,FUN = mean)
write.csv(cyclistic_summary, file = 'avg_ride_length.csv')

# Install flexdashboard for visualization dashboard
install.packages("flexdashboard")
library(flexdashboard)







