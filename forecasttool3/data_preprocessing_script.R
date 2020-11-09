rm(list = ls())

library(tidyverse)
library(dplyr)

float <- read_csv("/Users/jarellano/Desktop/float-people-20201106-134128-376d.csv", skip = 5)
float <- as.data.frame(float)
float <- float[!(is.na(float$`Job Title`)), ]
float <- float[ -c(4:8) ]
float <- aggregate( float[,4:379], float[,1:3], FUN = sum )
float <- pivot_longer(float, c(4:379), names_to = 'Date', values_to = 'float_Billable_Hours')
float$Date <- dmy(float$Date)
float$Date <- ymd(float$Date)

float_by_jobrole <- float[ -c(1, 3) ]
float_by_jobrole['Capacity'] = 8
float_by_jobrole$`Job Title` <- paste(float$`Job Title`, " - ", float$`Department`)
float_by_jobrole <- aggregate( float_by_jobrole[,3:4], float_by_jobrole[,1:2], FUN = sum )

connectwise <- read_csv("/Users/jarellano/Desktop/TimeEntries3.csv", trim_ws = TRUE)
connectwise <- connectwise[ , c(3:6)]
colnames(connectwise) <- c("first_Name","last_Name", "Date", "billable_Hours")
connectwise$Name <- paste(connectwise$first_Name, connectwise$last_Name)
connectwise <- connectwise[ , c(3:5)]
connectwise <- aggregate(connectwise$billable_Hours, by=list(Name=connectwise$Name, Date = connectwise$Date), FUN=sum)
connectwise$Name[connectwise$Name == "Benjamin Walters"] <- "Ben Walters"
connectwise$Name[connectwise$Name == "Dilip Gajendran"] <- "Dilip Raj Gajendran"
connectwise$Name[connectwise$Name == "Juan Arellano Marquez"] <- "Juan Arellano"
connectwise$Name[connectwise$Name == "Julisa Ricart Gubbins"] <- "Julisa Ricart"
connectwise$Name[connectwise$Name == "Miguel Mendoza-Gomez"] <- "Miguel Mendoza"
connectwise$Date = as.Date(connectwise$Date,format="%m/%d/%Y")
connectwise$Date <- ymd(connectwise$Date)
connectwise <- connectwise[(connectwise$Name %in% float$Name), ]

#write.csv(connectwise,"/Users/jarellano/Documents/GitHub/ksmcforecasttool/forecasttool3/connectwise.csv", row.names = FALSE)
#write.csv(float,"/Users/jarellano/Documents/GitHub/ksmcforecasttool/forecasttool3/float.csv", row.names = FALSE)
#write.csv(float_by_jobrole,"/Users/jarellano/Documents/GitHub/ksmcforecasttool/forecasttool3/float_by_jobrole.csv", row.names = FALSE)
