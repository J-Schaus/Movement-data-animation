######################################################
#   Animating GPS data                              ##
#   Multiple individuals                            ##
#   Script Author: Jessica Schaus                   ##
#   Date: 07/2020                                   ##
#   SPDX-License-Identifier: GPL-3.0-or-later       ##
#   GNU GPLv3+ © 2020 Jessica Schaus                ##
######################################################
#Load packages
library(moveVis)
library(move)
library(ggplot2)
library(dplyr)
library(readxl)
#For further details, please refer to the package moveVis information (https://rdrr.io/github/16EAGLE/moveVis/)
#Upload GPS data, containing ID of individuals (ID),  location (Latitude/Longitude), the DateTime (dt) of the fixes 
GPS <-read_excel("GPSdata.xlsx")

#Asign data set to dataframe class and the variables to the appropiate format
GPS<- as.data.frame(GPS)
GPS$dt <- as.POSIXct(GPS$dt, format="%d/%m/%Y %H:%M:%S", tz = "UTC")
GPS$ID<-as.factor(GPS$ID)


str(GPS)
#`'data.frame':	1409 obs. of  4 variables:
#`$ ID       : Factor w/ 2 levels "Female1","Male1": 1 1 1 1 1 1 1 1 1 1 ...
#`$ Latitude : num  53.8 53.8 53.8 53.8 53.8 ...
#`$ Longitude: num  -0.502 -0.502 -0.501 -0.501 -0.501 ...
#`$ dt       : POSIXct, format: "2019-08-16 22:50:07" "2019-08-16 22:55:12"


#Convert to a MoveStack class
GPS<-df2move(GPS, proj =CRS("+proj=longlat +ellps=WGS84"),
             x = "Longitude" , y = "Latitude", time = "dt", track_id = "ID")

#Have a look at  timestamps and sampling rates
unique(timestamps(GPS))
timeLag(GPS, unit = "mins")

#Sampling rate is roughly 5 minutes, but rates differ over time (i.e. tracks do not share unique timestamps). 
#For animation, unique frame times are needed, regardless of whether we want to animate a single or multiple tracks at once. 
#Align movement data to a uniform time scale with a uniform temporal resolution.

move_data <- align_move(GPS, res = "min", digit = 0, unit = "mins") #"min" use the smallest temporal resolution (default)

get_maptypes() #different types of baselines that can be used

#Create frames of spatial movement maps for animation
frames <- frames_spatial(move_data, path_colours = c("red", "cyan"), #indicates the color of the tracks
                         map_service = "osm", map_type = "streets", alpha = 1)#alpha= numeric background transparency (0-1).

length(frames) # number of frames
frames[[100]] #extract a frame to verify everything looks fine

#If map_service is other than "osm", a map_token is needed.
#Register at https://www.mapbox.com/ to get a mapbox token. Mapbox is free of charge after registration for up to 50.000 map requests per month.


frames <- frames_spatial(move_data, path_colours = c("red", "cyan"), 
                         map_service = "mapbox", map_type = "satellite",
                         map_token = "pk.eyJ1IjoianNjaGF1czg4IiwiYSI6ImNrMHdqOHoydDA0azQzbW5ib2Y3cDBsanAifQ.y_sv6WxNdD7QuwkXjWU7PQ")
frames[[100]] # check

# Customize the animation
frames <- add_labels(frames, title = "GPS data", x = "Longitude", y = "Latitude") %>% 
  add_progress() %>% 
  add_scalebar(distance = 0.2, colour = "white", height = 0.022, position = "bottomright", label_margin = 1.4) %>% #distance needs added for small scale maps (<1 km)
  add_northarrow(colour = "white", height = 0.08, position = "bottomleft") %>% 
  add_timestamps(move_data, type = "label")


frames[[10]] # check

suggest_formats() #shows the available formats to create the animation

#Export animation

#It's good to only export one set of the frames at first (e.g. the first 50 frames) to verify everything is correct
#as exporting the whole dataset takes time
animate_frames(frames[1:50], out_file = "MovementAnimation.mov",
               height = 500, width = 800, res = 100) #indicate dimentions and resolution for the videoclip

#Export all the frames
animate_frames(frames, out_file = "MovementAnimation.mov",
               height = 500, width = 800, res = 100)

####    END   ####
