#challenge life
require(magrittr)
library(dplyr)
require(stringi)
library(ggmap)

Farmers_Markets_in_New_York_State <- read.csv("~/Desktop/tuthill bot/Farmers_Markets_in_New_York_State.csv")

#need to adjust get map

farm_filter<-  filter(Farmers_Markets_in_New_York_State, City == "New York")

health_dataset<-bind_rows(farm_filter, data_subset) %>%
  mutate(subway= ifelse(is.na(LINE), FALSE, TRUE))

######################################################

health_dataset <- read.csv("~/Desktop/tuthill bot/Farmers_Markets_in_New_York_State.csv")

fun <- function() 
{
  longit <- as.numeric(readline("Enter a GPS Longitude:"))
  lat <-as.numeric(readline("Enter a GPS Latitude:"))
  length_x <-as.numeric(readline("Enter a distance (between .02 and .04):"))
  zu=matrix(0, nrow(health_dataset), 5)
  zu[,1]<-as.numeric(health_dataset$Longitude)
  zu[,2]<-as.numeric(health_dataset$Latitude)
  #zu[,3]<-as.character(health_dataset$NAME)
  
for(mu in 1:(nrow(health_dataset)))
{
    y=as.numeric(as.character(longit-health_dataset[mu,17]))
    y2=as.numeric(lat-health_dataset[mu, 16])
    distance=(round(sqrt(y^2 + y2^2), 3))
    zu[mu, 4]<-as.numeric((as.character(distance)))
}
  
  zoltron <- as.data.frame(subset(zu, zu[,4]  < length_x))
  colnames(zoltron) <- c("col1","col2","col3","col4","col5")
  
  nyc <- get_map(location = c(lon= longit, lat=lat), zoom = 'auto')
  add_lines <- function(long_vect, lat_vect)
  {
    annotate("segment", x=longit, xend= long_vect, y=lat, yend=lat_vect, size=1, colour="blue") 
  }
  ggmap(nyc) + geom_point(data=zoltron, aes(x = col1, y = col2, size = 2))  + add_lines(zoltron$col1, zoltron$col2)
}

zup <- if(interactive()) fun()
############################

