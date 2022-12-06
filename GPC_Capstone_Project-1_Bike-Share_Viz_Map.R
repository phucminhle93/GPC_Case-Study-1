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

##########LOCATIONS##########
# MOST POPULAR STATION
## START STATION - ALL
### [Isabella Peel] Create a sequence of values which will act as the key shown on the leaflet map to group stations which have a similar number of trips occurring together
mybins_startstation <- seq(0, 77040, by = 12840)

### Prepare map title (see: https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map)

# tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title { 
#     transform: translate(-50%,20%);
#     position: fixed !important;
#     left: 50%;
#     text-align: center;
#     padding-left: 10px; 
#     padding-right: 10px; 
#     background: rgba(255,255,255,0.75);
#     font-weight: bold;
#     font-size: 15px;
#   }
# "))
# 
# title <- tags$div(
#   tag.map.title, HTML("Most popular stations by number of departures")
# )  

### [Isabella Peel] Assign the viridis colour palette to visually show how popular a station is 
mypalette <- colorBin(
  palette ="viridis",
  domain = rides_v_startstation_all$numtrips,
  na.color = "transparent", 
  bins = mybins_startstation
)

### [Isabella Peel] Prepare text to be used in a tooltip so that users can interact with the coloured markers on the map
mytext <- paste(
  "Station name: ", rides_v_startstation_all$start_station_name, "<br/>",
  "Number of trips: ", rides_v_startstation_all$numtrips, sep = "" 
) %>%
  lapply(htmltools::HTML)

### Create an interactive html leaflet widget to show the most popular stations
pop_startstation <- SharedData$new(rides_v_startstation_all)
pop_startstation_map <- bscols(
  widths = c(12),
  device = c("lg"),
  list(
    filter_slider("numtrips", 
                  "Number of trips/departures", 
                  pop_startstation, 
                  column=~numtrips,
                  min = 0,
                  step=50, 
                  width=200
                  ), # add numtrips slider
    leaflet(pop_startstation) %>%
      addTiles() %>%
      setView(lng = -87.6298, lat = 41.8781, zoom = 12.0 
              ) %>% # Set coordinates over the city of Chicago
      addProviderTiles("Esri.WorldGrayCanvas") %>% #### Set map style
      addCircleMarkers(
        ~ start_lng, ~ start_lat, 
        fillColor = ~ mypalette(numtrips), 
        fillOpacity = 0.7, 
        color = "white", 
        radius = 6, 
        stroke = FALSE,
        label = mytext,
        labelOptions = labelOptions(
          style = list( 
            "font-weight" = "normal", 
            padding = "3px 8px"
          ), 
          textsize = "13px", 
          direction = "auto"
        ) 
      ) %>% # Add circle markers to represent each station 
      # & add a fill colour to show the popularity of each station 
      # & add an interactive tooltip for detail
      addLegend( 
        pal = mypalette, 
        values = ~ numtrips, 
        opacity = 0.8,
        title = "Trips/Departures - All Riderships", 
        position = "bottomright"
      ), # Add a legend
      # %>% addControl(title, position = "topleft", className="map-title") # Add Title
    datatable(pop_startstation, 
              extensions="Scroller", 
              width="100%",
              class="compact cell-border", 
              options=list(columnDefs = list(list(visible=FALSE, targets=c(2,3)), list(width = "10%", targets=c(1))),
                           deferRender=TRUE, 
                           scrollY=300, 
                           scroller=TRUE)
              ) # Add data table
    )
  )
pop_startstation_map

#############################################
## END STATION - ALL

### Prepare map title
# title <- tags$div(
#   tag.map.title, HTML("Most popular stations by number of arrivals")
# )  

### [Isabella Peel] Create a sequence of values which will act as the key shown on the leaflet map to group stations which have a similar number of trips occurring together
mybins_endstation <- seq(0, 78600, by = 13100)

### [Isabella Peel] Assign the viridis colour palette to visually show how popular a station is 
mypalette <- colorBin(
  palette ="viridis",
  domain = rides_v_endstation_all$numtrips,
  na.color = "transparent", 
  bins = mybins_endstation
)

### [Isabella Peel] Prepare text to be used in a tooltip so that users can interact with the coloured markers on the map
mytext <- paste(
  "Station name: ", rides_v_endstation_all$end_station_name, "<br/>",
  "Number of trips: ", rides_v_endstation_all$numtrips, sep = "" 
) %>%
  lapply(htmltools::HTML)

### Create an interactive html leaflet widget to show the most popular stations
pop_endstation <- SharedData$new(rides_v_endstation_all)
pop_endstation_map <- bscols(
  widths = c(12),
  device = c("lg"),
  list(
    filter_slider("numtrips", 
                  "Number of trips/arrivals", 
                  pop_endstation, 
                  column=~numtrips,
                  min = 0,
                  step=50, 
                  width=200
    ), # add numtrips slider
    leaflet(pop_endstation) %>%
      addTiles() %>%
      setView(lng = -87.6298, lat = 41.8781, zoom = 12.0 
      ) %>% # Set coordinates over the city of Chicago
      addProviderTiles("Esri.WorldGrayCanvas") %>% #### Set map style
      addCircleMarkers(
        ~ end_lng, ~ end_lat, 
        fillColor = ~ mypalette(numtrips), 
        fillOpacity = 0.7, 
        color = "white", 
        radius = 6, 
        stroke = FALSE,
        label = mytext,
        labelOptions = labelOptions(
          style = list( 
            "font-weight" = "normal", 
            padding = "3px 8px"
          ), 
          textsize = "13px", 
          direction = "auto"
        ) 
      ) %>% # Add circle markers to represent each station 
      # & add a fill colour to show the popularity of each station 
      # & add an interactive tooltip for detail
      addLegend( 
        pal = mypalette, 
        values = ~ numtrips, 
        opacity = 0.8,
        title = "Trips/Arrivals - All Riderships", 
        position = "bottomright"
      ), # Add a legend
    # %>% addControl(title, position = "topleft", className="map-title") # Add Title
    datatable(pop_endstation, 
              extensions="Scroller", 
              width="100%",
              class="compact cell-border",
              options=list(columnDefs = list(list(visible=FALSE, targets=c(2,3)), 
                                             list(width = "10%", targets=c(1))),
                           deferRender=TRUE, 
                           scrollY=300, 
                           scroller=TRUE)
    ) # Add data table
  )
)
pop_endstation_map

#############################################
## START STATION - MEMBER

### [Isabella Peel] Create a sequence of values which will act as the key shown on the leaflet map to group stations which have a similar number of trips occurring together
mybins_startstation_mem <- seq(0, 25320, by = 4220)

### [Isabella Peel] Assign the viridis colour palette to visually show how popular a station is 
mypalette <- colorBin(
  palette ="viridis",
  domain = rides_v_startstation_member$numtrips,
  na.color = "transparent", 
  bins = mybins_startstation_mem
)

### [Isabella Peel] Prepare text to be used in a tooltip so that users can interact with the coloured markers on the map
mytext <- paste(
  "Station name: ", rides_v_startstation_member$start_station_name, "<br/>",
  "Number of trips: ", rides_v_startstation_member$numtrips, sep = "" 
) %>%
  lapply(htmltools::HTML)

### Create an interactive html leaflet widget to show the most popular stations
pop_startstation_mem <- SharedData$new(rides_v_startstation_member)
pop_startstation_mem_map <- bscols(
  widths = c(12),
  device = c("lg"),
  list(
    filter_slider("numtrips", 
                  "Number of trips/departures", 
                  pop_startstation_mem, 
                  column=~numtrips,
                  min = 0,
                  step=50, 
                  width=200
    ), # add numtrips slider
    leaflet(pop_startstation_mem) %>%
      addTiles() %>%
      setView(lng = -87.6298, lat = 41.8781, zoom = 12.0 
      ) %>% # Set coordinates over the city of Chicago
      addProviderTiles("Esri.WorldGrayCanvas") %>% #### Set map style
      addCircleMarkers(
        ~ start_lng, ~ start_lat, 
        fillColor = ~ mypalette(numtrips), 
        fillOpacity = 0.7, 
        color = "white", 
        radius = 6, 
        stroke = FALSE,
        label = mytext,
        labelOptions = labelOptions(
          style = list( 
            "font-weight" = "normal", 
            padding = "3px 8px"
          ), 
          textsize = "13px", 
          direction = "auto"
        ) 
      ) %>% # Add circle markers to represent each station 
      # & add a fill colour to show the popularity of each station 
      # & add an interactive tooltip for detail
      addLegend( 
        pal = mypalette, 
        values = ~ numtrips, 
        opacity = 0.8,
        title = "Trips/Departures - Members", 
        position = "bottomright"
      ), # Add a legend
    # %>% addControl(title, position = "topleft", className="map-title") # Add Title
    datatable(pop_startstation_mem, 
              extensions="Scroller", 
              width="100%",
              class="compact cell-border",
              options=list(columnDefs = list(list(visible=FALSE, targets=c(3,4)), 
                                             list(width = "5%", targets=c(1))),
                           deferRender=TRUE, 
                           scrollY=300, 
                           scroller=TRUE)
    ) # Add data table
  )
)
pop_startstation_mem_map

#############################################
##END STATION - MEMBER

### [Isabella Peel] Create a sequence of values which will act as the key shown on the leaflet map to group stations which have a similar number of trips occurring together
mybins_endstation_mem <- seq(0, 25320, by = 4220)

### [Isabella Peel] Assign the viridis colour palette to visually show how popular a station is 
mypalette <- colorBin(
  palette ="viridis",
  domain = rides_v_endstation_member$numtrips,
  na.color = "transparent", 
  bins = mybins_endstation_mem
)

### [Isabella Peel] Prepare text to be used in a tooltip so that users can interact with the coloured markers on the map
mytext <- paste(
  "Station name: ", rides_v_endstation_member$end_station_name, "<br/>",
  "Number of trips: ", rides_v_endstation_member$numtrips, sep = "" 
) %>%
  lapply(htmltools::HTML)

### Create an interactive html leaflet widget to show the most popular stations
pop_endstation_mem <- SharedData$new(rides_v_endstation_member)
pop_endstation_mem_map <- bscols(
  widths = c(12),
  device = c("lg"),
  list(
    filter_slider("numtrips", 
                  "Number of trips/arrivals", 
                  pop_endstation_mem, 
                  column=~numtrips,
                  min = 0,
                  step=50, 
                  width=200
    ), # add numtrips slider
    leaflet(pop_endstation_mem) %>%
      addTiles() %>%
      setView(lng = -87.6298, lat = 41.8781, zoom = 12.0 
      ) %>% # Set coordinates over the city of Chicago
      addProviderTiles("Esri.WorldGrayCanvas") %>% #### Set map style
      addCircleMarkers(
        ~ end_lng, ~ end_lat, 
        fillColor = ~ mypalette(numtrips), 
        fillOpacity = 0.7, 
        color = "white", 
        radius = 6, 
        stroke = FALSE,
        label = mytext,
        labelOptions = labelOptions(
          style = list( 
            "font-weight" = "normal", 
            padding = "3px 8px"
          ), 
          textsize = "13px", 
          direction = "auto"
        ) 
      ) %>% # Add circle markers to represent each station 
      # & add a fill colour to show the popularity of each station 
      # & add an interactive tooltip for detail
      addLegend( 
        pal = mypalette, 
        values = ~ numtrips, 
        opacity = 0.8,
        title = "Trips/Arrivals - Members", 
        position = "bottomright"
      ), # Add a legend
    # %>% addControl(title, position = "topleft", className="map-title") # Add Title
    datatable(pop_endstation_mem, 
              extensions="Scroller", 
              width="100%",
              class="compact cell-border",
              options=list(columnDefs = list(list(visible=FALSE, targets=c(3,4)), 
                                             list(width = "10%", targets=c(1))),
                           deferRender=TRUE, 
                           scrollY=300, 
                           scroller=TRUE)
    ) # Add data table
  )
)
pop_endstation_mem_map

#############################################
## START STATION - CASUAL

### [Isabella Peel] Create a sequence of values which will act as the key shown on the leaflet map to group stations which have a similar number of trips occurring together
mybins_startstation_cas <- seq(0, 60360, by = 10060)

### [Isabella Peel] Assign the viridis colour palette to visually show how popular a station is 
mypalette <- colorBin(
  palette ="viridis",
  domain = rides_v_startstation_casual$numtrips,
  na.color = "transparent", 
  bins = mybins_startstation_cas
)

### [Isabella Peel] Prepare text to be used in a tooltip so that users can interact with the coloured markers on the map
mytext <- paste(
  "Station name: ", rides_v_startstation_casual$start_station_name, "<br/>",
  "Number of trips: ", rides_v_startstation_casual$numtrips, sep = "" 
) %>%
  lapply(htmltools::HTML)

### Create an interactive html leaflet widget to show the most popular stations
pop_startstation_cas <- SharedData$new(rides_v_startstation_casual)
pop_startstation_cas_map <- bscols(
  widths = c(12),
  device = c("lg"),
  
  list(
    filter_slider("numtrips", 
                  "Number of trips/departures", 
                  pop_startstation_cas, 
                  column=~numtrips,
                  min = 0,
                  step=50, 
                  width=200
    ), # add numtrips slider
    leaflet(pop_startstation_cas) %>%
      addTiles() %>%
      setView(lng = -87.6298, lat = 41.8781, zoom = 12.0 
      ) %>% # Set coordinates over the city of Chicago
      addProviderTiles("Esri.WorldGrayCanvas") %>% #### Set map style
      addCircleMarkers(
        ~ start_lng, ~ start_lat, 
        fillColor = ~ mypalette(numtrips), 
        fillOpacity = 0.7, 
        color = "white", 
        radius = 6, 
        stroke = FALSE,
        label = mytext,
        labelOptions = labelOptions(
          style = list( 
            "font-weight" = "normal", 
            padding = "3px 8px"
          ), 
          textsize = "13px", 
          direction = "auto"
        ) 
      ) %>% # Add circle markers to represent each station 
      # & add a fill colour to show the popularity of each station 
      # & add an interactive tooltip for detail
      addLegend( 
        pal = mypalette, 
        values = ~ numtrips, 
        opacity = 0.8,
        title = "Trips/Departures - Casual Riders", 
        position = "bottomright"
      ), # Add a legend
    # %>% addControl(title, position = "topleft", className="map-title") # Add Title
    datatable(pop_startstation_cas, 
              extensions="Scroller", 
              width="100%",
              class="compact cell-border",
              options=list(columnDefs = list(list(visible=FALSE, targets=c(3,4)), 
                                             list(width = "5%", targets=c(1))),
                           deferRender=TRUE, 
                           scrollY=300, 
                           scroller=TRUE)
    ) # Add data table
  )
)
pop_startstation_cas_map

#############################################
##END STATION - CASUAL

### [Isabella Peel] Create a sequence of values which will act as the key shown on the leaflet map to group stations which have a similar number of trips occurring together
mybins_endstation_cas <- seq(0, 63240, by = 10540)

### [Isabella Peel] Assign the viridis colour palette to visually show how popular a station is 
mypalette <- colorBin(
  palette ="viridis",
  domain = rides_v_endstation_casual$numtrips,
  na.color = "transparent", 
  bins = mybins_endstation_cas
)

### [Isabella Peel] Prepare text to be used in a tooltip so that users can interact with the coloured markers on the map
mytext <- paste(
  "Station name: ", rides_v_endstation_casual$end_station_name, "<br/>",
  "Number of trips: ", rides_v_endstation_casual$numtrips, sep = "" 
) %>%
  lapply(htmltools::HTML)

### Create an interactive html leaflet widget to show the most popular stations
pop_endstation_cas <- SharedData$new(rides_v_endstation_casual)
pop_endstation_cas_map <- bscols(
  widths = c(12),
  device = c("lg"),
  
  list(
    filter_slider("numtrips", 
                  "Number of trips/arrivals", 
                  pop_endstation_cas, 
                  column=~numtrips,
                  min = 0,
                  step=50, 
                  width=200
    ), # add numtrips slider
    leaflet(pop_endstation_cas) %>%
      addTiles() %>%
      setView(lng = -87.6298, lat = 41.8781, zoom = 12.0 
      ) %>% # Set coordinates over the city of Chicago
      addProviderTiles("Esri.WorldGrayCanvas") %>% #### Set map style
      addCircleMarkers(
        ~ end_lng, ~ end_lat, 
        fillColor = ~ mypalette(numtrips), 
        fillOpacity = 0.7, 
        color = "white", 
        radius = 6, 
        stroke = FALSE,
        label = mytext,
        labelOptions = labelOptions(
          style = list( 
            "font-weight" = "normal", 
            padding = "3px 8px"
          ), 
          textsize = "13px", 
          direction = "auto"
        ) 
      ) %>% # Add circle markers to represent each station 
      # & add a fill colour to show the popularity of each station 
      # & add an interactive tooltip for detail
      addLegend( 
        pal = mypalette, 
        values = ~ numtrips, 
        opacity = 0.8,
        title = "Trips/Arrivals - Casual Riders", 
        position = "bottomright"
      ), # Add a legend
    # %>% addControl(title, position = "topleft", className="map-title") # Add Title
    datatable(pop_endstation_cas, 
              extensions="Scroller", 
              width="100%",
              class="compact cell-border",
              options=list(columnDefs = list(list(visible=FALSE, targets=c(3,4)), 
                                             list(width = "10%", targets=c(1))),
                           deferRender=TRUE, 
                           scrollY=300, 
                           scroller=TRUE)
    ) # Add data table
  )
)
pop_endstation_cas_map
