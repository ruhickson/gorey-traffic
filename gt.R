# libraries to include
library(gifski)
library(gganimate)
library(tidyr)
library(transformr)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(hms)

# data sourced from https://data.gov.ie/dataset/gorey-traffic-data/resource/9ab6de86-e690-417a-bafc-8b9acd4936c1?inner_span=True

# access data
setwd("./gorey-road-traffic-data/")
getwd()

# combine all CSV files together
file_list <- list.files()
file_list

setwd("..")

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE, sep=",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE, sep=",")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

# explore dataset
str(dataset)
head(dataset)

# create workable subset
dataset_mini <- dataset[,c("Date","Time","Direction_West","Direction_East")]

# handle datetime and round to nearest 15 minutes
dataset_mini$datetime <- as.POSIXct(as.character(paste(dataset_mini$Date, dataset_mini$Time)))
dataset_mini$datetime <- round_date(dataset_mini$datetime, "15 minutes")
dataset_mini$Time <- as_hms(dataset_mini$datetime)

# verify change worked
head(dataset_mini)

# create time-series dataset
timedata <- dataset_mini %>%
  group_by(Time) %>%
  summarise(W = sum(Direction_West), E = -sum(Direction_East)) %>% 
  as.data.frame()

# reshape data from wide to long
timedata <- timedata %>% gather(., key=Direction, value=no_of_cars, c("W","E"))
timedata$Time <- as_hms(timedata$Time)

# plot time-series
p <- ggplot(timedata) +
  geom_area(aes(x=Time,y=no_of_cars,fill=Direction)) +
  theme_tufte() +
  labs(title = paste0("Gorey Road Traffic by Hour of the Day (",stdate," to ",endate,")")) +
  xlab("Time of Day") +
  ylab("<- East          West ->")

p

# animate time-series
p + transition_reveal(Time)

# save latest animation locally as a gif
anim_save("gorey.gif")

