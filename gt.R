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

setwd("..")

# explore dataset
str(dataset)
head(dataset)

# create workable subset
dataset_mini <- dataset[,c("Date","Time","Direction_West","Direction_East")]

# handle Datetime and round to nearest 15 minutes
dataset_mini$Datetime <- as.POSIXct(as.character(paste(dataset_mini$Date, dataset_mini$Time)))
dataset_mini$Datetime <- round_date(dataset_mini$Datetime, "15 minutes")
dataset_mini$Time <- as_hms(dataset_mini$Datetime)

# verify change worked
head(dataset_mini)

# create time-series dataset
timedata <- dataset_mini %>%
  group_by(Time) %>%
  summarise(W = sum(Direction_West), E = -sum(Direction_East)) %>% 
  as.data.frame()

# reshape data from wide to long
timedata1 <- timedata %>% gather(., key=Direction, value=no_of_cars, c("W","E"))
timedata1$Time <- as_hms(timedata1$Time)

stdate <- min(dataset_mini$Date)
endate <- max(dataset_mini$Date)

# plot time-series
ggplot(timedata1) +
  geom_col(aes(x=Time, y=-no_of_cars, fill=Direction)) +
  theme_tufte() +
  labs(title = paste0("Gorey Road Traffic by Time of Day (",stdate," to ",endate,")")) +
  xlab("Time of Day") +
  ylab("<- West          East ->") +
  coord_flip() +
  scale_fill_manual(values = c("purple","gold"))

# Calculate E-W Flow throughout the day
timedata$Flow <- timedata$W + timedata$E

# Plot Flow
ggplot(timedata) +
  geom_text(aes(x=Time, y=-Flow, size = -Flow), label = ">") +
  geom_col(aes(x=Time, y=-Flow, fill=-Flow), alpha=.4) +
  theme_tufte() +
  labs(title = paste0("Gorey Road Traffic Flow by Time the Day (",stdate," to ",endate,")")) +
  xlab("Time of Day") +
  ylab("<- West          East ->") +
  coord_flip() +
  theme(legend.position = "none")


# create time-series dataset
datedata <- dataset_mini %>%
  group_by(Date) %>%
  summarise(W = sum(Direction_West), E = -sum(Direction_East)) %>% 
  as.data.frame()

# reshape data from wide to long
datedata1 <- datedata %>% gather(., key=Direction, value=no_of_cars, c("W","E"))
datedata1$Date <- as.POSIXct.Date(datedata1$Date)
str(datedata1)

# plot and animate time-series
ggplot(datedata1) +
  geom_col(aes(x=Date, y=-no_of_cars, fill=Direction)) +
  theme_tufte() +
  labs(title = paste0("Gorey Road Traffic by Date (",stdate," to ",endate,")")) +
  xlab("Date") +
  ylab("<- West          East ->") +
  scale_fill_manual(values = c("purple","gold")) +
  transition_states(Date) +
  shadow_mark()

# save latest animation locally as a gif
anim_save("gorey.gif")

save(dataset, file = "dataset.Rdata")
