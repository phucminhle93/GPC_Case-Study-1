#Install and load packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("geosphere")
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(skimr)
library(janitor)
library(dplyr)
library(geosphere)


# Remember: crtl+L : clear console, ctrl + shift + C: turn multiple lines to comments
# Change and view working directory
getwd() #view path of current (initial) working dir
setwd("C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1")
getwd() #view path of working dir
list.dirs() #view content of working dir

# import all rides data (and assign to variable) for the 12 months specified
rides_2207 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202207-divvy-tripdata.csv")
rides_2206 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202206-divvy-tripdata.csv")
rides_2205 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202205-divvy-tripdata.csv")
rides_2204 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202204-divvy-tripdata.csv")
rides_2203 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202203-divvy-tripdata.csv")
rides_2202 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202202-divvy-tripdata.csv")
rides_2201 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202201-divvy-tripdata.csv")
rides_2112 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202112-divvy-tripdata.csv")
rides_2111 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202111-divvy-tripdata.csv")
rides_2110 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202110-divvy-tripdata.csv")
rides_2109 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202109-divvy-tripdata.csv")
rides_2108 <- read.csv(file = "GPC_Capstone_Project-1_Bike-Share_Data/202108-divvy-tripdata.csv")

#clean data (loosely follow https://www.r-bloggers.com/2021/04/how-to-clean-the-datasets-in-r/)
# also reference to finished project by isabellapeel: https://github.com/isabellapeel/Cyclistic_Case_Study-Divvy_Bikes/commit/66ee32b787763aee61b021fe253ad620786a1fbc)

# test (quickly) if dataframes have the same columns and number of columns before performing rbind

colnames(rides_2207)
ncol(rides_2207)

# check dataframe structure (column type) of each individual, raw dataframe
glimpse(rides_2207)
glimpse(rides_2206)
glimpse(rides_2205)
glimpse(rides_2204)
glimpse(rides_2203)
glimpse(rides_2202)
glimpse(rides_2201)
glimpse(rides_2112)
glimpse(rides_2111)
glimpse(rides_2110)
glimpse(rides_2109)
glimpse(rides_2108)

# [Inspection] Summarize the data types of each raw dataframe in a single dataframe

coltype_rides_2207 <- data.frame(Type_rides_2207=sapply(rides_2207,class)) # no need for a separate column for column name like the command above
coltype_rides_2206 <- data.frame(Type_rides_2206=sapply(rides_2206,class))
coltype_rides_2205 <- data.frame(Type_rides_2205=sapply(rides_2205,class))
coltype_rides_2204 <- data.frame(Type_rides_2204=sapply(rides_2204,class))
coltype_rides_2203 <- data.frame(Type_rides_2203=sapply(rides_2203,class))
coltype_rides_2202 <- data.frame(Type_rides_2202=sapply(rides_2202,class))
coltype_rides_2201 <- data.frame(Type_rides_2201=sapply(rides_2201,class))
coltype_rides_2112 <- data.frame(Type_rides_2112=sapply(rides_2112,class))
coltype_rides_2111 <- data.frame(Type_rides_2111=sapply(rides_2111,class))
coltype_rides_2110 <- data.frame(Type_rides_2110=sapply(rides_2110,class))
coltype_rides_2109 <- data.frame(Type_rides_2109=sapply(rides_2109,class))
coltype_rides_2108 <- data.frame(Type_rides_2108=sapply(rides_2108,class))

coltype_all_df <- cbind(coltype_rides_2207, coltype_rides_2206, 
                        coltype_rides_2205, coltype_rides_2204,  
                        coltype_rides_2203, coltype_rides_2202,
                        coltype_rides_2201, coltype_rides_2112,
                        coltype_rides_2111, coltype_rides_2110,  
                        coltype_rides_2109, coltype_rides_2108
                        )
View(coltype_all_df)

coltype_all_df_transposed <- t(coltype_all_df)
View(coltype_all_df_transposed) 

# [Only for 2020 data]
# change start_station_id and end_station_id in rides_2011 - rides_2008 
# to characters:

# rides_2011 <- mutate(rides_2011, 
#                      start_station_id = as.character(start_station_id),
#                      end_station_id = as.character(end_station_id)
#                      )
# rides_2010 <- mutate(rides_2010, 
#                      start_station_id = as.character(start_station_id),
#                      end_station_id = as.character(end_station_id)
#                      )
# rides_2009 <- mutate(rides_2011, 
#                      start_station_id = as.character(start_station_id),
#                      end_station_id = as.character(end_station_id)
#                      )
# rides_2008 <- mutate(rides_2011, 
#                      start_station_id = as.character(start_station_id),
#                      end_station_id = as.character(end_station_id)
#                      )

# coltype_rides_2011 <- data.frame(Type_rides_2011=sapply(rides_2011,class))
# coltype_rides_2010 <- data.frame(Type_rides_2010=sapply(rides_2010,class))
# coltype_rides_2009 <- data.frame(Type_rides_2009=sapply(rides_2009,class))
# coltype_rides_2008 <- data.frame(Type_rides_2008=sapply(rides_2008,class))

# coltype_df_2008_2011 <- cbind(coltype_rides_2011, coltype_rides_2010, 
#                               coltype_rides_2009, coltype_rides_2008
#                               )
# View(coltype_df_2008_2011)
# coltype_df_2008_2011_transposed <- t(coltype_df_2008_2011)
# View(coltype_df_2008_2011_transposed) 

# create lists of all loaded dataframes
rides_dfnames_2022 <- list("rides_2207" = rides_2207, "rides_2206" = rides_2206,
                           "rides_2205" = rides_2205, "rides_2204" = rides_2204,
                           "rides_2203" = rides_2203, "rides_2202" = rides_2202,
                           "rides_2201" = rides_2201
                           )
rides_dfnames_2021 <- list("rides_2112" = rides_2112, "rides_2111" = rides_2111,
                           "rides_2110" = rides_2110, "rides_2109" = rides_2109,
                           "rides_2108" = rides_2108
                           )

rides_dfnames <- list("rides_2207" = rides_2207, "rides_2206" = rides_2206, 
                      "rides_2205" = rides_2205, "rides_2204" = rides_2204, 
                      "rides_2203" = rides_2203,"rides_2202" = rides_2202,
                      "rides_2201" = rides_2201, "rides_2112" = rides_2112, 
                      "rides_2111" = rides_2111, "rides_2110" = rides_2110, 
                      "rides_2109" = rides_2109, "rides_2108" = rides_2108
                      )

# stack these dataframes to form a single dataframe
rides_masterdf_2022 <- rbind(rides_2207, rides_2206, rides_2205, rides_2204, 
                             rides_2203, rides_2202, rides_2201
                             )


rides_masterdf_2021 <- rbind(rides_2112, rides_2111, rides_2110, rides_2109, 
                             rides_2108
                             )

rides_masterdf <- rbind(rides_masterdf_2022, rides_masterdf_2021)
glimpse(rides_masterdf)

#Resulting names are unique and consist only of the "_" character, numbers, and letters. 
#Capitalization preferences can be specified using the case parameter.
rides_masterdf_cleaned <- clean_names(rides_masterdf) #clean_names for ride_masterdf

for (rides_df in rides_dfnames) {
  rides_df <- clean_names(rides_df)
  } #clean_names for each of the rides files/dataframes

# Check Occurences/Frequency of things (including blank cells)
tabyl(rides_masterdf_cleaned, "start_station_name")
tabyl(rides_masterdf_cleaned, "start_lat")
tabyl(rides_masterdf_cleaned, "rideable_type")

# Remove empty rows and columns
rides_masterdf_cleaned <- rides_masterdf_cleaned %>% 
  remove_empty(whic=c("rows"))

rides_masterdf_cleaned <- rides_masterdf_cleaned %>% 
  remove_empty(whic=c("cols"))

# Remove ride id duplicates ( improved from isabella peel)
ride_id_check <- rides_masterdf_cleaned %>%
  count(ride_id) %>%
  filter(n > 1)
head(ride_id_check)
glimpse(ride_id_check)

rides_masterdf_cleaned <- rides_masterdf_cleaned %>%
  distinct(ride_id, .keep_all = TRUE) 
  #ref source: https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/

ride_id_check_nodup <- rides_masterdf_cleaned %>%
  count(ride_id) %>%
  filter(n > 1)
head(ride_id_check_nodup)
glimpse(ride_id_check_nodup)

# Remove rows with unknown station names (isabella peel)
rides_masterdf_cleaned <- rides_masterdf_cleaned %>%
  filter(
    !(is.na(start_station_name) | start_station_name == "")
  ) %>% 
  
  filter(
    !(is.na(end_station_name) | end_station_name == "")
  )

# Remove test rides (isabella peel)
capitalized_station_name_check <- rides_masterdf_cleaned %>% 
  filter(
    str_detect(start_station_name, "[:upper:]") & !str_detect(start_station_name,"[:lower:]")
  ) %>%
  
  group_by(
    start_station_name
  ) %>%
  
  count(
    start_station_name
  )

rides_masterdf_cleaned <- rides_masterdf_cleaned %>%
  filter(
    !(str_detect(start_station_name, "[:upper:]")
      & !str_detect(start_station_name, "[:lower:]"))
  )

# Change start and end time to date/time class/type (isabella peel)
  # Change start_at string type (isabella peel)
  rides_masterdf_cleaned$started_at <- as.POSIXct(
    rides_masterdf_cleaned$started_at, format = "%Y-%m-%d %H:%M:%S"
    ) 
  # Question: why not use mutate?
  
  # Change ended_at string type (isabella peel)
  rides_masterdf_cleaned$ended_at <- as.POSIXct(
    rides_masterdf_cleaned$ended_at, format = "%Y-%m-%d %H:%M:%S"
  )
  
  #probably a shorter code for doing both of the above at the same time
  rides_masterdf_cleaned <- rides_masterdf_cleaned %>%
    mutate(started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
           ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S")
           )
  
  # Order by date 
  rides_masterdf_cleaned <- rides_masterdf_cleaned %>%
    arrange(started_at)

# Calculate ride duration (isabella peel)
  rides_masterdf_cleaned$ride_duration <- difftime(
    rides_masterdf_cleaned$ended_at, 
    rides_masterdf_cleaned$started_at,
    units = "secs"
  ) 
  
  # Change string type to numeric 
  rides_masterdf_cleaned$ride_duration <- as.numeric(
    as.character(rides_masterdf_cleaned$ride_duration)
    )
# Remove rows with negative ride duration (isabella peel)
rides_masterdf_cleaned <- rides_masterdf_cleaned %>%
  filter(!(ride_duration < 0))


# Remove rows with no coordinates for start and end stations
rides_masterdf_cleaned <- rides_masterdf_cleaned %>%
  filter(!is.na(start_lng) | !is.na(start_lat) | !is.na(end_lng) | !is.na(end_lat))

# Calculate ride distance 
# NOTE: if a distance is 0, DO NOT remove it! riders may make a round trip!

rides_masterdf_cleaned <- rides_masterdf_cleaned %>%
  rowwise() %>% 
  mutate(ride_distance = distHaversine(cbind(start_lng, start_lat), 
                                       cbind(end_lng, end_lat)
                                       )
         )

gc()
# Save cleaned data frame
fwrite(
  rides_masterdf_cleaned, 
  "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/rides_masterdf_cleaned_221030.csv", 
  col.names = TRUE,
  row.names = FALSE
  )

fwrite(
  rides_masterdf, 
  "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/rides_masterdf_221030.csv", 
  col.names = TRUE,
  row.names = FALSE
  )

fwrite(
  coltype_all_df_transposed, 
  "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/coltype_all_df_transposed_221030.csv", 
  col.names = TRUE,
  row.names = FALSE
  )

fwrite(
  ride_id_check, 
  "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/ride_id_check_221030.csv", 
  col.names = TRUE,
  row.names = FALSE
  )

fwrite(
  ride_id_check_nodup, 
  "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/ride_id_check_nodup_221030.csv", 
  col.names = TRUE,
  row.names = FALSE
  )

fwrite(
  capitalized_station_name_check, 
  "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/capitalized_station_name_check_221030.csv", 
  col.names = TRUE,
  row.names = FALSE
  )