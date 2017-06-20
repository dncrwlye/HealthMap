#challenge life
require(magrittr)
library(dplyr)
require(stringi)
library(ggmap)
library(leaflet)
library(httr)
library(RSocrata)
############
#Bring in Data
############

#Farmers_Markets <- read.socrata("https://data.ny.gov/resource/7jkw-gj56.json")
#read.socrata isn't working 

gps_cleaner=function(data)
{
  mutate(data, Longitude=as.numeric(stri_extract_first_regex(the_geom, "-[0-9][0-9].[0-9]+"))) %>%
  mutate(Latitude=as.numeric(stri_extract_last_regex(the_geom, "(?<=\\s{0,2})[0-9][0-9]\\.[0-9]{5}")))
}

Farmers_Markets_in_New_York_State <- read.csv("~/Desktop/health_map/Farmers_Markets_in_New_York_State.csv")
#need to adjust get map
farm_filter<-  filter(Farmers_Markets_in_New_York_State, (County == "Queens"| County == "New York" | County == "Bronx"))

DOITT_SUBWAY_ENTRANCE_01_13SEPT2010 <- read.csv("~/Desktop/health_map/DOITT_SUBWAY_ENTRANCE_01_13SEPT2010.csv")
data_subset <- gps_cleaner(DOITT_SUBWAY_ENTRANCE_01_13SEPT2010)

#################
#CLEAN DATASET
#################

health_dataset<-bind_rows(farm_filter, data_subset) %>%
  mutate(subway= ifelse(is.na(LINE), FALSE, TRUE)) %>%
  mutate(NAME = ifelse(is.na(NAME) & !is.na(Market.Name), as.character(Market.Name), as.character(NAME)))

######################################################
#GET ICONS
######################################################


fun <- function() 
{
  #longit <- as.numeric(readline("Enter a GPS Longitude:"))
  #lat <-as.numeric(readline("Enter a GPS Latitude:"))
  #length_x <-as.numeric(readline("Enter a distance (between .02 and .04):"))
  longit <- -73.947
  lat <- 40.808
  length_x <-.4
  
  farm_filter <- readline("Do you want to include farmer's markets? (yes or no):")
    #if ((farm_filter !="yes") & (farm_filter !="no"))
    if (grepl("[Y|y]es", farm_filter)== FALSE & grepl("[N|n]o", farm_filter)== FALSE)
     {print("not a valid response")}
  subway_filter <- readline("Do you want to include subways? (yes or no):")
    #if ((subway_filter !="yes") & (subway_filter !="no"))
  if (grepl("[Y|y]es", subway_filter)== FALSE & grepl("[N|n]o", subway_filter)== FALSE)
      {print("not a valid response")}
  if (farm_filter == "no")
    {
     health_dataset <- health_dataset %>% 
       filter(subway != FALSE) 
    }
  if (subway_filter == "no")
    {
    health_dataset <- health_dataset %>% filter(subway != TRUE) 
    }
  
  distance_matrix=matrix(0, nrow(health_dataset), 6)
  distance_matrix[,1]<-as.numeric(health_dataset$Longitude)
  distance_matrix[,2]<-as.numeric(health_dataset$Latitude)
  distance_matrix[,3]<-as.character(health_dataset$NAME)
  distance_matrix[,6]<-as.character(health_dataset$subway)
for(mu in 1:(nrow(health_dataset)))
{
    y=as.numeric(as.character(longit-health_dataset[mu,17]))
    y2=as.numeric(lat-health_dataset[mu, 16])
    distance=(round(sqrt(y^2 + y2^2), 3))
    distance_matrix[mu, 4]<-as.numeric((as.character(distance)))
}
  distance_matrix_reduced <- as.data.frame(subset(distance_matrix, distance_matrix[,4]  < length_x))
  colnames(distance_matrix_reduced) <- c("col1","col2","col3","col4","col5", "col6")
  
  
  mapIcons <- icons(iconUrl = ifelse(distance_matrix_reduced$col6 == FALSE,
                                     "https://d30y9cdsu7xlg0.cloudfront.net/png/13611-200.png",
                                     "http://www.freeiconspng.com/uploads/subway-icon-png-21.png"), iconWidth = 14, iconHeight = 15)
  
map<-leaflet(distance_matrix_reduced) %>%
  addTiles() %>%
  addMarkers (lng = ~col1, lat = ~col2, popup = ~col3, icon = mapIcons) 
map
}

distance_matrix <- if(interactive()) fun()
############################
#not changing anything

#-73.98   -73.929224     -73.947
#40.759    40.75478     40.808





