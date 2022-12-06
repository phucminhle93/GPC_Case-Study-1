#Install and load packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("geosphere")
# install.packages("viridis")
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(skimr)
library(janitor)
library(dplyr)
library(geosphere) # functions for calculating distances based on earth coordinates
# library(viridis) # color maps
library(ggpubr) # added functions to ggplot2

# Remember: crtl+L : clear console, ctrl + shift + C: turn multiple lines to comments
# Change and view working directory
getwd() #view path of current (initial) working dir
setwd("C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1")
getwd() #view path of working dir
list.dirs() #view content of working dir

# Load file (use fread instead of read.csv; same outcome but faster)
rides_cleaned <- fread("C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/rides_masterdf_cleaned_221030.csv")
glimpse(rides_cleaned)

# Add columns to deconstruct time data
  # Year 
  rides_cleaned$started_at_year <- format(rides_cleaned$started_at, "%Y")
  
  # Month 
  rides_cleaned$started_at_month <- format(rides_cleaned$started_at, "%m")
  
  # Week 
  rides_cleaned$started_at_week <- format(rides_cleaned$started_at,"%W")
  
  # Day
  rides_cleaned$started_at_day <- format(rides_cleaned$started_at, "%d")
  
  # Day of week 
  rides_cleaned$started_at_weekday <- format(rides_cleaned$started_at, "%A")
  
  # Date, YYYY-MM-DD
  rides_cleaned$started_at_YMD <- format(rides_cleaned$started_at, "%Y-%m-%d")
  ## Convert the YMD variable to a date format (dttm) (for later inner join with weather data)
  rides_cleaned$started_at_YMD_POSIX <- as.POSIXct(rides_cleaned$started_at_YMD, format = "%Y-%m-%d")
  
  # Time of Day, HH:MM:SS
  rides_cleaned$started_at_ToD <- format(rides_cleaned$started_at, "%H:%M:%S")
  ## Convert the time of day variable to a date format (for later ToD analysis)
  rides_cleaned$started_at_ToD_POSIX <- as.POSIXct(rides_cleaned$started_at_ToD, format = "%H:%M:%S")
  ## Group the time variable by hours in rides_cleaned
  rides_cleaned$started_at_ToD_byHr <- cut(
    rides_cleaned$started_at_ToD_POSIX, 
    breaks = "60 mins"
  )
  glimpse(rides_cleaned)


##########CREATE DATAFRAMES FOR COUNTING TRIPS##########
  
# create new dataframes from rides_cleaned with just rides and ridership 
## grouped by memeber type
rides_v_riderships <- rides_cleaned %>%
  
  select(
    member_casual 
  ) %>%
  
  group_by(member_casual) %>%
  
  mutate(numtrips = n()) %>%
  
  distinct(member_casual, .keep_all = TRUE)
  
  rides_v_riderships <-rides_v_riderships[order(rides_v_riderships$numtrips,
                                                decreasing = TRUE)
                                          ] %>% # sort by numtrips
    mutate(prop = numtrips / sum(rides_v_riderships$numtrips) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop ) # for label positions in possible pie chart
    
  
glimpse(rides_v_riderships)
  
# create new dataframes from rides_cleaned with just rides and time info 
## grouped by memeber type and started_at_month (redundant, but for practice)
  rides_v_month <- rides_cleaned %>%
    
    select(
      member_casual, 
      started_at,
      started_at_year, 
      started_at_month, 
      started_at_week, 
      started_at_day, 
      started_at_weekday, 
      started_at_YMD,
      started_at_YMD_POSIX,
      started_at_ToD,
      started_at_ToD_POSIX,
      started_at_ToD_byHr,
      ended_at,
      ride_duration
    ) %>%
    
    group_by(member_casual, started_at_month) %>%
    
    mutate(numtrips = n()) %>%
    
    distinct(started_at_month, member_casual, .keep_all = TRUE)
  
  glimpse(rides_v_month)
  
  ## Create a rides_v_month dataframe for member riders only 
  rides_v_month_member <- rides_v_month %>%
    filter(member_casual == "member")
  
  glimpse(rides_v_month_member)
  
  ## Create a rides_v_month dataframe for casual riders only
  rides_v_month_casual <- rides_v_month %>%
    filter(member_casual == "casual")
  
  glimpse(rides_v_month_casual)


# create new dataframes from rides_cleaned with just rides and time info 
## grouped by memeber type and started_at_YMD
rides_v_YMD <- rides_cleaned %>%
  
  select(
    member_casual, 
    started_at,
    started_at_year, 
    started_at_month, 
    started_at_week, 
    started_at_day, 
    started_at_weekday, 
    started_at_YMD,
    started_at_YMD_POSIX,
    started_at_ToD,
    started_at_ToD_POSIX,
    started_at_ToD_byHr,
    ended_at,
    ride_duration
  ) %>%
  
  group_by(member_casual, started_at_YMD) %>%
  
  mutate(numtrips = n()) %>%
  
  distinct(started_at_YMD, member_casual, .keep_all = TRUE)

glimpse(rides_v_YMD)

## Create a rides_v_YMD dataframe for member riders only 
rides_v_YMD_member <- rides_v_YMD %>%
  filter(member_casual == "member")

glimpse(rides_v_YMD_member)

## Create a rides_v_YMD dataframe for casual riders only
rides_v_YMD_casual <- rides_v_YMD %>%
  filter(member_casual == "casual")

glimpse(rides_v_YMD_casual)


# create new dataframes from rides_cleaned with just rides and time info 
## grouped by memeber type and started_at_ToD
rides_v_hr <- rides_cleaned %>%
  
  select(
    member_casual, 
    started_at,
    started_at_year, 
    started_at_month, 
    started_at_week, 
    started_at_day, 
    started_at_weekday, 
    started_at_YMD,
    started_at_YMD_POSIX,
    started_at_ToD,
    started_at_ToD_POSIX,
    started_at_ToD_byHr,
    ended_at,
    ride_duration
  ) %>%
  
  group_by(member_casual, started_at_ToD_byHr) %>%
  
  mutate(numtrips = n()) %>%
  
  distinct(started_at_ToD_byHr, member_casual, .keep_all = TRUE)

glimpse(rides_v_hr)

## Create a rides_v_hr dataframe for member riders only 
rides_v_hr_member <- rides_v_hr %>%
  filter(member_casual == "member")

glimpse(rides_v_hr_member)

## Create a rides_v_hr dataframe for casual riders only
rides_v_hr_casual <- rides_v_hr %>%
  filter(member_casual == "casual")

glimpse(rides_v_hr_casual)

# create new dataframes from rides_cleaned with just rides and rideable_type
## grouped by memeber type and rideable_type
rides_v_type <- rides_cleaned %>%
  select(
    member_casual, 
    rideable_type
  ) %>%
  
  group_by(member_casual, rideable_type) %>%
  
  mutate(numtrips = n()) %>%
  
  distinct(rideable_type, member_casual, .keep_all = TRUE)

glimpse(rides_v_type)

## Create a rides_v_type dataframe for member riders only 
rides_v_type_member <- rides_v_type %>%
  filter(member_casual == "member")

glimpse(rides_v_type_member)

## Create a rides_v_type dataframe for casual riders only
rides_v_type_casual <- rides_v_type %>%
  filter(member_casual == "casual")

glimpse(rides_v_type_casual)

#Load raw weather data
## Data from https://www.ncdc.noaa.gov/cdo-web/datatools/records
raw_weather <- fread("C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Weather-Data/Weather_ZIP60666_210731-220831.csv")
glimpse(raw_weather)

## Re-format date column (in a new column) to dttm and chr. Note that the original DATE column is already of type "Date"
raw_weather <- raw_weather %>%
  mutate(date_POSIX = as.POSIXct(DATE, format = "%Y-%m-%d")) %>%  #The new column has type dttm
  mutate(date_chr = as.character(DATE))
glimpse(raw_weather)

#reorganize data in raw_weather & change column names
weather_tavg_prcp_wnd <- raw_weather %>% 
  select(
    DATE,
    date_chr,
    date_POSIX,
    TAVG,
    PRCP,
    AWND,
  )
colnames(weather_tavg_prcp_wnd)<- c("started_at_YMD_date", "started_at_YMD", "started_at_YMD_POSIX", "avg_temp", "precipitation", "wind_speed")
glimpse(weather_tavg_prcp_wnd)

#inner join rides_v_YMD with weather_tavg_prcp_wnd
rides_v_weather <- merge(x=rides_v_YMD,y=weather_tavg_prcp_wnd,by="started_at_YMD") #join by dates as chr. Join by date as dttm does NOT work for some reasons
glimpse(rides_v_weather)

##########CREATE DATAFRAMES GROUPED BY LOCATIONS & DURATION##########
# create new dataframes from rides_cleaned with just rides and start station
## grouped by start stations
rides_v_startstation_all <- rides_cleaned %>%
  select(
    start_station_name, 
    start_lat, 
    start_lng
  ) %>%
  
  group_by(start_station_name) %>%
  
  mutate(numtrips = n()) %>%
  
  distinct(start_station_name,  .keep_all = TRUE)

### Sort busiest start station
rides_v_startstation_all <-rides_v_startstation_all[order(rides_v_startstation_all$numtrips, decreasing = TRUE),] 
glimpse(rides_v_startstation_all)

## grouped by member type and start stations
rides_v_startstation <- rides_cleaned %>%
  select(
    member_casual,
    start_station_name, 
    start_lat, 
    start_lng
  ) %>%
  
  group_by(member_casual, start_station_name) %>%
  
  mutate(numtrips = n()) %>%
  
  distinct(member_casual, start_station_name,  .keep_all = TRUE)

glimpse(rides_v_startstation)

## Create a rides_v_startstation dataframe for member riders only 
rides_v_startstation_member <- rides_v_startstation %>%
  filter(member_casual == "member")

### Sort busiest start station for members
rides_v_startstation_member <-rides_v_startstation_member[order(rides_v_startstation_member$numtrips, decreasing = TRUE),] 
glimpse(rides_v_startstation_member)

## Create a rides_v_startstation dataframe for casual riders only
rides_v_startstation_casual <- rides_v_startstation %>%
  filter(member_casual == "casual")

### Sort busiest start station for casual riders
rides_v_startstation_casual <-rides_v_startstation_casual[order(rides_v_startstation_casual$numtrips, decreasing = TRUE),] 
glimpse(rides_v_startstation_casual)

# create new dataframes from rides_cleaned with just rides and end stations
## grouped by end stations
rides_v_endstation_all <- rides_cleaned %>%
  select(
    end_station_name, 
    end_lat, 
    end_lng
  ) %>%
  
  group_by(end_station_name) %>%
  
  mutate(numtrips = n()) %>%
  
  distinct(end_station_name,  .keep_all = TRUE)

### Sort busiest end station
rides_v_endstation_all <-rides_v_endstation_all[order(rides_v_endstation_all$numtrips, decreasing = TRUE),] 
glimpse(rides_v_endstation_all)

## grouped by member type and end stations
rides_v_endstation <- rides_cleaned %>%
  select(
    member_casual,
    end_station_name, 
    end_lat, 
    end_lng
  ) %>%
  
  group_by(member_casual, end_station_name) %>%
  
  mutate(numtrips = n()) %>%
  
  distinct(member_casual, end_station_name,  .keep_all = TRUE)

glimpse(rides_v_endstation)

## Create a rides_v_endstation dataframe for member riders only 
rides_v_endstation_member <- rides_v_endstation %>%
  filter(member_casual == "member")

### Sort busiest end station for members
rides_v_endstation_member <-rides_v_endstation_member[order(rides_v_endstation_member$numtrips, decreasing = TRUE),] 
glimpse(rides_v_endstation_member)

## Create a rides_v_endstation dataframe for casual riders only
rides_v_endstation_casual <- rides_v_endstation %>%
  filter(member_casual == "casual")

### Sort busiest end station for causal riders
rides_v_endstation_casual <-rides_v_endstation_casual[order(rides_v_endstation_casual$numtrips, decreasing = TRUE),] 
glimpse(rides_v_endstation_casual)

# create new dataframes from rides_cleaned with rides, ride distance, duration, among others (note: ride_duration in seconds)
rides_duration_distance <- rides_cleaned %>%
  select(
    member_casual, 
    started_at,
    started_at_year, 
    started_at_month, 
    started_at_week, 
    started_at_day, 
    started_at_weekday, 
    started_at_YMD,
    started_at_YMD_POSIX,
    started_at_ToD,
    started_at_ToD_POSIX,
    started_at_ToD_byHr,
    ended_at,
    ride_duration,
    ride_distance
  )
### mean duration (sec) and mean distance (m) per month by ridership type
rides_duration_distance_perMonth <- rides_duration_distance%>%
  group_by(member_casual, started_at_month) %>%
  summarise_at(.var = c("ride_duration", "ride_distance"),
               .funs = c(mean="mean", Sd="sd"))

### mean duration and mean distance per weekday by ridership type
rides_duration_distance_perWeekday <- rides_duration_distance%>%
  group_by(member_casual, started_at_weekday) %>%
  summarise_at(.var = c("ride_duration", "ride_distance"),
               .funs = c(mean="mean", Sd="sd"))

### mean duration and mean distance by hour by ridership type
rides_duration_distance_byHr <- rides_duration_distance%>%
  group_by(member_casual, started_at_ToD_byHr) %>%
  summarise_at(.var = c("ride_duration", "ride_distance"),
               .funs = c(mean="mean", Sd="sd"))  
  
# ## Create a rides_duration_distance dataframe for members only
# rides_duration_distance_members <- rides_duration_distance %>%
#   filter(member_casual == "member")
# 
# ### mean duration and mean distance by members per month
# rides_duration_distance_perMonth_members <- rides_duration_distance_members%>%
#   group_by(started_at_month) %>%
#   summarise_at(.var = c("ride_duration", "ride_distance"),
#                .funs = c(mean="mean", Sd="sd"))
# 
# ### mean duration and mean distance by members per weekday
# rides_duration_distance_perWeekday_members <- rides_duration_distance_members%>%
#   group_by(started_at_weekday) %>%
#   summarise_at(.var = c("ride_duration", "ride_distance"),
#                .funs = c(mean="mean", Sd="sd"))
# 
# ### mean duration and mean distance by members by hour
# rides_duration_distance_byHr_members <- rides_duration_distance_members%>%
#   group_by(started_at_ToD_byHr) %>%
#   summarise_at(.var = c("ride_duration", "ride_distance"),
#                .funs = c(mean="mean", Sd="sd"))  
# 
# ## Create a rides_duration_distance dataframe for casual riders only
# rides_duration_distance_casual <- rides_duration_distance %>%
#   filter(member_casual == "casual")
# 
# ### mean duration and mean distance by casual riders per month
# rides_duration_distance_perMonth_casual <- rides_duration_distance_casual%>%
#   group_by(started_at_month) %>%
#   summarise_at(.var = c("ride_duration", "ride_distance"),
#                .funs = c(mean="mean", Sd="sd"))
# 
# ### mean duration and mean distance by casual riders per weekday
# rides_duration_distance_perWeekday_casual <- rides_duration_distance_casual%>%
#   group_by(started_at_weekday) %>%
#   summarise_at(.var = c("ride_duration", "ride_distance"),
#                .funs = c(mean="mean", Sd="sd"))
# 
# ### mean duration and mean distance by casual riders by hour
# rides_duration_distance_byHr_casual <- rides_duration_distance_casual%>%
#   group_by(started_at_ToD_byHr) %>%
#   summarise_at(.var = c("ride_duration", "ride_distance"),
#                .funs = c(mean="mean", Sd="sd"))

## Check for round trip
rides_roundtrips <- rides_duration_distance %>%
  filter(ride_distance == 0 && ride_duration > 0)

glimpse(rides_roundtrips) #no round trips found


##########SAVE PROCESSED DATAFRAMES##########
df_names <- c("rides_cleaned",
              
              "rides_v_riderships",
              
              "rides_v_YMD",
              "rides_v_YMD_casual",
              "rides_v_YMD_member",
              
              "rides_v_hr",
              "rides_v_hr_casual",
              "rides_v_hr_member",
              
              "rides_v_month",
              "rides_v_month_casual",
              "rides_v_month_member",
              
              "rides_v_type",
              "rides_v_type_casual",
              "rides_v_type_member",
              
              "rides_v_weather",
              
              "rides_v_startstation",
              "rides_v_startstation_all",
              "rides_v_startstation_casual",
              "rides_v_startstation_member",
              
              "rides_v_endstation",
              "rides_v_endstation_all",
              "rides_v_endstation_casual",
              "rides_v_endstation_member",
              
              "rides_duration_distance",
              "rides_duration_distance_perMonth",
              "rides_duration_distance_byHr",
              "rides_duration_distance_perWeekday"
              ) 
for(i in 1:length(df_names)) {                          # Head of for-loop
  fwrite(get(df_names[i]),                              # Write CSV files to folder
             paste0("C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/",
                    df_names[i],
                    ".csv"),
         col.names = TRUE,    
         row.names = FALSE)
}