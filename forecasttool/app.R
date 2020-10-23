library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(anytime)

query <- "https://api.float.com/v3/people"
getdata<-GET(url = query, user_agent("Juan's Data Pull (juan.arellano@orrfellowship.org)"), add_headers(Authorization = "Bearer 14b4584fab7e7d93Wgwo6Ma6E+7qS9WmNgsZEsx8OEcZWJmonzI4Ck5E9A4="))
df = fromJSON(content(getdata,type="text"))


query <- "https://api.float.com/v3/tasks?start_date=2020-10-15&end_date=2020-10-31"
getdata<-GET(url = query, user_agent("Juan's Data Pull (juan.arellano@orrfellowship.org)"), add_headers(Authorization = "Bearer 14b4584fab7e7d93Wgwo6Ma6E+7qS9WmNgsZEsx8OEcZWJmonzI4Ck5E9A4="))
sched = fromJSON(content(getdata,type="text"))

sched$start_date <- anydate(as.factor(sched$start_date))

team_load = aggregate(hours ~ start_date, sched, sum)

df$people_id
df$name



juan <- subset(sched, people_id == 17452182)
amrutha <- subset(sched, people_id == 17452186)
hannah <- subset(sched, people_id == 17453781)
alexa <- subset(sched, people_id == 17453782)
sandip <- subset(sched, people_id == 17453783)


Juan_load = aggregate(hours ~ start_date, juan, sum)
Amrutha_load = aggregate(hours ~ start_date, amrutha, sum)
Hannah_load = aggregate(hours ~ start_date, hannah, sum)
Alexa_load = aggregate(hours ~ start_date, alexa, sum)
Sandip_load = aggregate(hours ~ start_date, sandip, sum)


# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("KSMC Hourly Forecast Tool"),
  
  radioButtons("hours", "Choose Department or Person:",
               c("Data Science Team","Juan", "Amrutha", "Alexa", "Hannah", "Sandip")),
  plotOutput("hourPlot")
)

server <- function(input, output) {
  output$hourPlot <- renderPlot({
    switch(input$hours,
           "Data Science Team" = plot(team_load$start_date, team_load$hours, type = "b",main="Data Science Overall Team",xlab="Date Week Starts With", ylab="Hours per Day", pch=19),
           "Juan" = plot(Juan_load$start_date, Juan_load$hours, main="Juan Arellano", type = "b", xlab="Date Week Starts With ", ylab="Hours per Day", pch=19),
           "Amrutha" = plot(Amrutha_load$start_date, Amrutha_load$hours, type = "b", main="Amrutha Wheeler",xlab="Date Week Starts With ", ylab="Hours per Day", pch=19),
           "Alexa" = plot(Alexa_load$start_date, Alexa_load$hours, type = "b", main="Alexa Kovacs", xlab="Date Week Starts With ", ylab="Hours per Day", pch=19),
           "Hannah" = plot(Hannah_load$start_date, Hannah_load$hours, type = "b", main="Hannah King",xlab="Date Week Starts With ", ylab="Hours per Day", pch=19),
           "Sandip" = plot(Sandip_load$start_date, Sandip_load$hours, type = "b", main="Sandip Biswas",xlab="Date Week Starts With ", ylab="Hours per Day", pch=19))
  })
}

shinyApp(ui = ui, server = server)