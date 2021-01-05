rm(list = ls())

library(tidyverse)
library(dplyr)
library(lubridate)


###Float API  LOGIN
#query <- "https://api.float.com/v3/people"
#getdata<-GET(url = query, user_agent("Juan's Data Pull (jarellano@ksmconsulting.com)"), add_headers(Authorization = "Bearer d6a28e0869479b80OM/zSctRJWMK1U2u+wXlgtkgt9zeSU1olcoRViW1Iz8="))
#df = fromJSON(content(getdata,type="text"))

#query <- "https://api.float.com/v3/tasks?start_date=2020-10-15&end_date=2021-05-31"
#getdata<-GET(url = query, user_agent("Juan's Data Pull (jarellano@ksmconsulting.com)"), add_headers(Authorization = "Bearer d6a28e0869479b80OM/zSctRJWMK1U2u+wXlgtkgt9zeSU1olcoRViW1Iz8="))
#sched = fromJSON(content(getdata,type="text"))

## ConnectWise Server Access
#library(odbc)
#con <- dbConnect(odbc(),
#                 Driver = "ODBC Driver 17 for SQL Server",
#                 Server = "KSMC-SQL01",
#                 Database = "cwdbwh")

#db <- DBI::dbConnect(odbc::odbc(),
 #                    Driver = 'ODBC Driver 17 for SQL Server',
 #                     Server = 'KSMC-SQL01',
  #                   Database = "cwdbwh",
  ##                   trusted_connection = 'yes',
   #                  uid = "KSMC/jarellano",
  #                   pwd ="*********")

#library(odbc)
#library(DBI)
#con <- DBI::dbConnect(odbc::odbc(),
 #                     Driver   = "ODBC Driver 17 for SQL Server",
  #                    Server   = "KSMC-SQL01",
   #                   Database = "cwdbwh",
    #                  UID      = "KSMC\\svcsql",
     #                 Trusted_Connection = "yes",
      #                Port     = 1433)

#library(DBI)

#con <- dbConnect(odbc::odbc(), 
 #                .connection_string = 'driver="ODBC Driver 17 for SQL Server";server="KSMC-SQL01";database="cwdbwh";trusted_connection=true')

# SQL Query
#SELECT pbi.time.[Member_RecID]
#, First_Name
#, Last_Name
#,[Date_Start]
#,[Billable_Hrs]
#FROM [cwdbwh].[pbi].[time]
#LEFT JOIN Member 
#ON pbi.time.Member_RecID = member.Member_RecID


#float <- read_csv("/Users/jarellano/Desktop/float.csv", skip = 5)
float <- as.data.frame(float)
float <- float[!(is.na(float$`Job Title`)), ]
float <- float[ -c(4:8) ]
#
#
# REMEMBER YOU HAVE TO CHANGE THIS EACH TIME YOU UPLOAD A NEW FLOAT FILE
#
#
#
float <- aggregate( float[,4:520], float[,1:3], FUN = sum )
float <- pivot_longer(float, c(4:520), names_to = 'Date', values_to = 'float_Billable_Hours')
float$Date <- dmy(float$Date)
float$Date <- ymd(float$Date)
dds <- float
dds$Department <- "All DDS"
float <- rbind(float, dds)
float <-float[!(float$Name == "Ikram Sarfraz"),]


float_by_jobrole <- float[ -c(1) ]
float_by_jobrole <-float_by_jobrole[!(float_by_jobrole$Department == "All DDS"),]
float_by_jobrole['Capacity'] = 8
float_by_jobrole$`Job Title` <- paste(float_by_jobrole$`Job Title`, " - ", float_by_jobrole$`Department`)
float_by_jobrole <- float_by_jobrole[ -c(2) ]
float_by_jobrole <- aggregate( float_by_jobrole[,3:4], float_by_jobrole[,1:2], FUN = sum )


#connectwise <- read_csv("/Users/jarellano/Desktop/TimeSeries4.csv", trim_ws = TRUE)
connectwise <- connectwise[ , c(2:5)]
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
