library(shiny)
library(httr)
library(jsonlite)
library(dplyr)

query <- "https://api.float.com/v3/people"
getdata<-GET(url = query, user_agent("Juan's Data Pull (juan.arellano@orrfellowship.org)"), add_headers(Authorization = "Bearer 14b4584fab7e7d93Wgwo6Ma6E+7qS9WmNgsZEsx8OEcZWJmonzI4Ck5E9A4="))
df = fromJSON(content(getdata,type="text"))


query <- "https://api.float.com/v3/tasks?start_date=2020-10-15&end_date=2020-10-31"
getdata<-GET(url = query, user_agent("Juan's Data Pull (juan.arellano@orrfellowship.org)"), add_headers(Authorization = "Bearer 14b4584fab7e7d93Wgwo6Ma6E+7qS9WmNgsZEsx8OEcZWJmonzI4Ck5E9A4="))
sched = fromJSON(content(getdata,type="text"))

team_load = aggregate(hours ~ start_date, sched, sum)

df$people_id
df$name

weeks <- c('Week 1', 'Week 2')

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
ui <- pageWithSidebar(
  headerPanel('KSMC Forecast Tool'),
  sidebarPanel(
    uiOutput("filter_degree")
    
  ),
  mainPanel(
    uiOutput('plot')
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$filter_degree<-renderUI({
    radioButtons("rd","Select Option",choices = c("Data Science Overall Team","Juan",'Amrutha', 'Hannah', 'Alexa', 'Sandip'),
                 selected = "Data Science Overall Team")
  })
  
  
  output$plot <- renderUI({
    if(input$rd=="Data Science Overall Team"){
      output$plot1<-renderPlot({
        
        plot(team_load$start_date, team_load$hours, main="Data Science Overall Team",
             xlab="Date Week Starts With", ylab="Hours per Day", pch=19)
      })
      plotOutput("plot1")
    }
    
    
    else if(input$rd=="Juan"){
      output$plot2<-renderUI({
        
        plot(Juan_load$start_date, Juan_load$hours, main="Juan Arellano",
             xlab="Date Week Starts With ", ylab="Hours per Day", pch=19)
      })
      plotOutput("plot2")
    }
    
    else if(input$rd=="Amrutha"){
      output$plot3<-renderUI({
        
        plot(Juan_load$start_date, Amrutha_load$hours, main="Amrutha Wheeler",
             xlab="Date Week Starts With ", ylab="Hours per Day", pch=19)
      })
      plotOutput("plot3")
    }
    
    else if(input$rd=="Hannah"){
      output$plot4<-renderUI({
        
        plot(Juan_load$start_date, Hannah_load$hours, main="Hannah King",
             xlab="Date Week Starts With ", ylab="Hours per Day", pch=19)
      })
      plotOutput("plot4")
    }
    
    else if(input$rd=="Alexa"){
      output$plot5<-renderUI({
        
        plot(Juan_load$start_date, Alexa_load$hours, main="Alexa Kovacs",
             xlab="Date Week Starts With ", ylab="Hours per Day", pch=19)
      })
      plotOutput("plot5")
    }
    
    else if(input$rd=="Sandip"){
      output$plot6<-renderUI({
        
        plot(Juan_load$start_date, Sandip_load$hours, main="Sandip Biswas",
             xlab="Date Week Starts With ", ylab="Hours per Day", pch=19)
      })
      plotOutput("plot6")
    }
  })
  
}

shinyApp(ui = ui, server = server)