#Install and load packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("geosphere")
install.packages("viridis")
install.packages("leaflet")
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(skimr)
library(janitor)
library(dplyr)
library(geosphere) # functions for calculating distances based on earth coordinates
library(viridis) # color maps
library(ggpubr) # added functions to ggplot2
library(leaflet) # for geographical map
library(crosstalk) # for sliders on geographical map
library(DT) #for visualized data tables 
library(htmlwidgets) # for widgets on maps
library(htmltools) #tools for creating, manipulating, and writing HTML from R

# Remember: crtl+L : clear console, ctrl + shift + C: turn multiple lines to comments
# Change and view working directory
getwd() #view path of current (initial) working dir
setwd("C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1")
getwd() #view path of working dir
list.dirs() #view content of working dir

# Load all dataframes from analysis (use fread instead of read.csv; same outcome but faster)
df_names <- df_names <- c("rides_cleaned",
                          
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

for(i in 1:length(df_names)){
  assign(df_names[i], 
         fread(paste0("C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/",
                      df_names[i],
                      ".csv")))}

##########RIDERSHIP OVERVIEW##########
# Ridership Pie Chart
rides_v_riderships_pie <- ggplot(rides_v_riderships, aes(x="", y=prop, fill=member_casual)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  
  geom_text(aes(label = paste0(format(round(prop, 2), nsmall = 2), " %"
                               )
                ), 
            position = position_stack(vjust = 0.5),
            color = c("black", "white"), size=4) +
  
  scale_fill_viridis_d(option = "viridis", labels = c("Casual Rider", "Member Rider"))+
  labs(fill = "Ridership Type")+
  labs(title = paste0("Ridership \n 08.01.2021 - 07.31.2022 \n",
                      "Total no. of trips: ",
                      formatC(sum(rides_v_riderships$numtrips), format="d", big.mark=",")
                      )
       )

rides_v_riderships_pie


# Ridership per Bike Type
rides_v_type_bar <- ggplot(data=rides_v_type, 
                           aes(x=member_casual, 
                               y=numtrips/1000, 
                               fill=rideable_type)) +
  geom_bar(stat="identity")+
  labs(title = "Ridership per Bike Type \n 08.01.2021 - 07.31.2022")+
  xlab("Ridership Type") + 
  ylab("Number of Trips (thousand)")+
  theme_light() +
  labs(fill = "Bike Type")+
  scale_fill_viridis_d(option = "viridis", labels = c("Classic Bike", "Docked Bike", "Electric Bike")) #use viridis color scheme

rides_v_type_bar

# Ridership Comaprison per Month
rides_v_month_bar <- ggplot(data=rides_v_month, 
                            aes(x=started_at_month, 
                                y=numtrips/1000, 
                                fill=member_casual)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title = "Monthly Ridership \n 08.01.2021 - 07.31.2022")+
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 12, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  xlab("Month") + 
  ylab("Number of Trips (thousand)")+
  theme_light() +
  scale_fill_viridis_d(labels = c("Casual Rider", "Member Rider")) + 
      #use viridis color scheme and edit legends labels
  labs(fill = "Ridership Type")

rides_v_month_bar

##########TIME OF YEAR##########
# Create a heat map to show most popular time of year for members  
rides_v_YMD_mem_heat <- ggplot(
  rides_v_YMD_member,
  aes(
    x = started_at_week, 
    y = factor(rides_v_YMD_member$started_at_weekday, 
               levels = c("Monday", "Tuesday", "Wednesday", 
                          "Thursday", "Friday", "Saturday", "Sunday"
                          )
               ), # arrange the weekday from Mon-Sun on y-axis
    fill = numtrips
  )
) + 
  
  # Use the viridis colour scheme to show the popularity of each day
  scale_fill_viridis(
    option = "D",
    direction = 1,
    name = "Number of Trips"
  ) +
  
  # Create a rectangular heat map
  geom_tile(
    colour = "white", 
    na.rm = FALSE
  ) + 
  
  # Separate the heat maps by year
  facet_wrap(
    "started_at_year", 
    ncol = 1
  ) + 
  
  # Reverse the y-axis so that the weekdays read vertically Monday to Sunday 
  scale_y_discrete(
    limits = rev
  ) +
  
  # Add x-axis labels to show the months of the year
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 52, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  
  # Set the light theme 
  theme_light() +
  
  # Remove any unnecessary labels 
  theme(
    axis.title = element_blank()
  ) +
  
  # Add a title 
  labs(title = "Daily Ridership by Member Riders \n 08.01.2021 - 07.31.2022")

# Create a heat map to show most popular time of year for casual riders  
rides_v_YMD_cas_heat <- ggplot(
  rides_v_YMD_casual,
  aes(
    x = started_at_week, 
    y = factor(rides_v_YMD_member$started_at_weekday, 
               levels = c("Monday", "Tuesday", "Wednesday", 
                          "Thursday", "Friday", "Saturday", "Sunday"
                          )
               ), # arrange the weekday from Mon-Sun on y-axis
    fill = numtrips
  )
) + 
  
  # Use the viridis colour scheme to show the popularity of each day
  scale_fill_viridis(
    option = "D",
    direction = 1,
    name = "Number of Trips",
  ) +
  
  # Create a rectangular heat map
  geom_tile(
    colour = "white", 
    na.rm = FALSE
  ) + 
  
  # Separate the heat maps by year
  facet_wrap(
    "started_at_year", 
    ncol = 1
  ) + 
  
  # Reverse the y-axis so that the weekdays read vertically Monday to Sunday 
  scale_y_discrete(
    limits = rev
  ) +
  
  # Add x-axis labels to show the months of the year
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 52, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  
  # Set the light theme 
  theme_light() +
  
  # Remove any unnecessary labels 
  theme(
    axis.title = element_blank()
  ) +
  
  # Add a title 
  labs(title = "Daily Ridership by Casual Riders \n 08.01.2021 - 07.31.2022")

# Combine the members only and casual riders only heat maps into one with one common legend 
rides_v_YMD_mem_cas_heat <- ggarrange(
  rides_v_YMD_mem_heat, 
  rides_v_YMD_cas_heat, 
  ncol = 1, 
  nrow = 2,
  common.legend = TRUE, 
  legend = "right"
)

rides_v_YMD_mem_cas_heat

##########TIME OF DAY##########
# Create a circular bar chart to show the popularity of each hour
##convert started_at_ToD_byHr back to character (otherwise annotate would not work)
rides_v_hr$started_at_ToD_byHr <- as.character(rides_v_hr$started_at_ToD_byHr)

rides_v_hr_circular <- ggplot(rides_v_hr) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:4) * 125),
    color = "lightgrey"
  ) + 
  
  # Create a stacked bar chart
  geom_bar(
    aes(
      x = started_at_ToD_byHr,
      y = numtrips/1000,
      fill = member_casual
    ), 
    stat="identity",
    position=position_dodge()
  )+
  
  # Create circular shape which starts in the mid-line
  coord_polar(start = -0.135, direction = 1) +
  ylim(-600, 500)+

  # Add x-axis labels
  annotate(
    x = 1,
    y = -50,
    label = "00:00",
    geom = "text",
    size = 2.5,
  )+ 
  annotate(
    x = 2,
    y = -50,
    label = "01:00",
    geom = "text",
    size = 3,
  )+
  annotate(
    x = 3,
    y = -50,
    label = "02:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 4,
    y = -50,
    label = "03:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 5,
    y = -50,
    label = "04:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x= 6,
    y=-50,
    label = "05:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 7,
    y = -50,
    label = "06:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 8,
    y = -50,
    label = "07:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 9,
    y = -50,
    label = "08:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 10,
    y = -50,
    label = "09:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 11,
    y = -50,
    label = "10:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 12,
    y = -50,
    label = "11:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 13,
    y = -50,
    label = "12:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 14,
    y = -50,
    label = "13:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 15,
    y = -50,
    label = "14:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 16,
    y = -50,
    label = "15:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 17,
    y = -50,
    label = "16:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 18,
    y = -50,
    label = "17:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 19,
    y = -50,
    label = "18:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 20,
    y = -50,
    label = "19:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 21,
    y = -50,
    label = "20:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 22,
    y = -50,
    label = "21:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 23,
    y = -50,
    label = "22:00",
    geom = "text",
    size = 2.5,
  ) +
  annotate(
    x = 24,
    y = -50,
    label = "23:00",
    geom = "text",
    size = 2.5,
  )+

  # Annotate y-axis scaling labels
  annotate(
    x = 24,
    y = 125,
    label = "125,000 trips",
    geom = "text",
    size = 3,
    angle = 15
  ) +
  annotate(
    x = 24,
    y = 250,
    label = "250,000 trips",
    geom = "text",
    size = 3,
    angle = 15
  ) +
  annotate(
    x = 24,
    y = 375,
    label = "375,000 trips",
    geom = "text",
    size = 3,
    angle = 15
  ) +
  # annotate(
  #   x = 24,
  #   y = 500,
  #   label = "500,000 trips",
  #   geom = "text",
  #   size = 2.5,
  #   angle = 15
  # ) + #no longer necessary with splitted bar chart

  # Use viridis colour scheme
  scale_fill_viridis_d(labels = c("Casual Rider", "Member Rider")) +

  # Set light theme
  theme_light() +

  # Set legend title
  labs(fill = "Ridership Type")+
  
  # Remove unnecessary labels
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
  )+
  
  # Add title
  labs(title = "Total Ridership by Time of Day \n 08.01.2021 - 07.31.2022")
  
rides_v_hr_circular

##########EFFECT OF WEATHER##########
gc()
#1. Plot average temperature vs number of trips per day
avg_temp <- ggplot(
  rides_v_weather, 
  aes(
    x = avg_temp,
    y = numtrips/1000,
    color = member_casual
  )
) + 
  
  geom_point(
    size = 2, alpha = 0.5
  ) +
  
  # Add title and axis labels 
  labs(
    title = "Effect of Temperature on Daily Ridership",
    x = "Average Daily Temperature (C)", 
    y = "No. of Trips (thousand)"
  ) +
  
  #Use viridis colour scheme 
  scale_color_viridis_d(labels = c("Casual Rider", "Member Rider")) +
  
  # Set light theme 
  theme_light() +
  
  # Remove legend title and center title
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  
  # Add legend title
  labs(color = "Ridership Type")

  
# 2. Plot average wind speed vs number of trips per day
avg_wdspd <- ggplot(
  rides_v_weather, 
  aes(
    x = wind_speed,
    y = numtrips/1000,
    color = member_casual
  )
) + 
  
  # Create scatter points 
  geom_point(
    size = 2, alpha = 0.5
  ) +
  
  # Add title and axis labels 
  labs(
    title = "Effect of Wind Speed on Daily Ridership", 
    x = "Average Daily Wind Speed (m/s)", 
    y = "No. of Trips (thousand)"
  ) +
  
  #Use viridis colour scheme 
  scale_color_viridis_d(labels = c("Casual Rider", "Member Rider")) +
  
  # Set light theme
  theme_light() +

  # Remove legend title and center title
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  
  # Add legend title
  labs(color = "Ridership Type")


# 3. Plot average precipitation vs number of trips per day
avg_precip <- ggplot(
  rides_v_weather, 
  aes(
    x = precipitation,
    y = numtrips/1000,
    color = member_casual
  )
) + 
  
  # Create scatter points 
  geom_point(
    size = 2, alpha = 0.5
  ) +
  
  # Add title and axis labels 
  labs(
    title = "Effect of Precipitation on Daily Ridership", 
    x = "Average Daily Precipitation (mm)", 
    y = "No. of Trips (thousand)"
  ) +
  
  #Use viridis colour scheme 
  scale_color_viridis_d(labels = c("Casual Rider", "Member Rider")) +
  
  # Set light theme 
  theme_light() +
  
  # Remove legend title and center title
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  
  # Add legend title
  labs(color = "Ridership Type")
  
# Combine all 3 plots into one 
rides_v_weather_scatter <- ggarrange(
  avg_temp, 
  avg_precip, 
  avg_wdspd, 
  ncol = 2, 
  nrow = 2,
  common.legend = TRUE, 
  legend = "bottom"
)
rides_v_weather_scatter

##########DURATIONS AND DISTANCES##########
# Ride duration
## Mean ride duration per Month by ridership type
rides_duration_perMonth_barM <- ggplot(data=rides_duration_distance_perMonth, 
                            aes(x=started_at_month, 
                                y=ride_duration_mean/60, 
                                fill=member_casual)) +
  geom_bar(stat="identity", position=position_dodge())+
  # geom_errorbar(aes(ymin=(ride_duration_mean-ride_duration_Sd)/60,
  #                   ymax=(ride_duration_mean+ride_duration_Sd)/60
  #                   ),
  #               width=.2
  #               )+
  labs(title = "Monthly Mean Ride Duration by Ridership Type \n 08.01.2021 - 07.31.2022")+
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 12, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  xlab("Month") + 
  ylab("Mean Ride Duration (min)")+
  theme_light() +
  scale_fill_viridis_d(labels = c("Casual Rider", "Member Rider")) + #use viridis color scheme
  labs(fill = "Ridership Type")

## Standard deviation of ride duration per Month by ridership type
rides_duration_perMonth_barSd <- ggplot(data=rides_duration_distance_perMonth, 
                                               aes(x=started_at_month, 
                                                   y=ride_duration_Sd/60, 
                                                   fill=member_casual)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title = "Standard Deviation of Monthly Ride Duration by Ridership Type \n 08.01.2021 - 07.31.2022")+
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 12, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  xlab("Month") + 
  ylab("Stdev Value (min)")+
  theme_light() +
  scale_fill_viridis_d(labels = c("Casual Rider", "Member Rider")) + #use viridis color scheme
  labs(fill = "Ridership Type")

## [Combine the graphs] Monthly ride duration situation by ridership type
rides_duration_perMonth_bar <- ggarrange(
  rides_duration_perMonth_barM, 
  rides_duration_perMonth_barSd, 
  ncol = 1, 
  nrow = 2,
  common.legend = TRUE, 
  legend = "bottom"
)
rides_duration_perMonth_bar

# Ride distance
## Mean ride distance per Month by ridership type
rides_distance_perMonth_barM <- ggplot(data=rides_duration_distance_perMonth, 
                                               aes(x=started_at_month, 
                                                   y=ride_distance_mean/1000, #convert m to km
                                                   fill=member_casual)) +
  geom_bar(stat="identity", position=position_dodge())+
  # geom_errorbar(aes(ymin=(ride_distance_mean-ride_distance_Sd)/1000,
  #                   ymax=(ride_distance_mean+ride_distance_Sd)/1000
  #                   ),
  #               width=.2
  #               )+
  labs(title = "Monthly Mean Ride Distance by Ridership Type \n 08.01.2021 - 07.31.2022")+
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 12, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  xlab("Month") + 
  ylab("Mean Ride Distance (km)")+
  theme_light() +
  scale_fill_viridis_d(labels = c("Casual Rider", "Member Rider")) + #use viridis color scheme
  labs(fill = "Ridership Type")

## Standard deviation of ride distance per Month by ridership type
rides_distance_perMonth_barSd <- ggplot(data=rides_duration_distance_perMonth, 
                                       aes(x=started_at_month, 
                                           y=ride_distance_Sd/1000, #convert m to km
                                           fill=member_casual)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title = "Standard Deviation of Monthly Ride Distance by Ridership Type \n 08.01.2021 - 07.31.2022")+
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 12, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  xlab("Month") + 
  ylab("Stdev Value (km)")+
  theme_light() +
  scale_fill_viridis_d(labels = c("Casual Rider", "Member Rider")) + #use viridis color scheme
  labs(fill = "Ridership Type")

## [Combine the graphs] Monthly ride distance situation by ridership type
rides_distance_perMonth_bar <- ggarrange(
  rides_distance_perMonth_barM, 
  rides_distance_perMonth_barSd, 
  ncol = 1, 
  nrow = 2,
  common.legend = TRUE, 
  legend = "bottom"
)
rides_distance_perMonth_bar

##########SAVE PLOTS##########
ggsave(
  "rides_v_riderships_pie.png",
  plot = rides_v_riderships_pie,
  device = NULL,
  path = "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Viz/Plots",
  scale = 3,
  bg = "white"
)

ggsave(
  "rides_v_month_bar.png",
  plot = rides_v_month_bar,
  device = NULL,
  path = "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Viz/Plots",
  scale = 3,
  bg = "white"
)

ggsave(
  "rides_v_type_bar.png",
  plot = rides_v_type_bar,
  device = NULL,
  path = "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Viz/Plots",
  scale = 3,
  bg = "white"
)

ggsave(
  "rides_v_YMD_mem_cas_heat.png",
  plot = rides_v_YMD_mem_cas_heat,
  device = NULL,
  path = "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Viz/Plots",
  scale = 5,
  bg = "white"
)

ggsave(
  "rides_v_hr_circular.png",
  plot = rides_v_hr_circular,
  device = NULL,
  path = "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Viz/Plots",
  scale = 5,
  bg = "white"
  )

ggsave(
  "rides_v_weather_scatter.png",
  plot = rides_v_weather_scatter,
  device = NULL,
  path = "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Viz/Plots",
  scale = 5,
  bg = "white"
)

ggsave(
  "rides_duration_perMonth_bar.png",
  plot = rides_duration_perMonth_bar,
  device = NULL,
  path = "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Viz/Plots",
  scale = 5,
  bg = "white"
)

ggsave(
  "rides_distance_perMonth_bar.png",
  plot = rides_distance_perMonth_bar,
  device = NULL,
  path = "C:/Users/phucm/OneDrive/My Stuffs/Academic Materials/Books and Materials_by time period/2018-2020_RWTH-AACHEN/01_ADDITIONAL CLASSES/Coursera_Google-CC_Data-Analytics/C08_Capstone/Case-Study-1/Viz/Plots",
  scale = 5,
  bg = "white"
)
