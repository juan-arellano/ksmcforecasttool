rm(list = ls())

library(tidyverse)
library(shiny)
library(dplyr)
library(data.table)
library(lubridate)
library(xts)
library(forecast)
library(prophet)


float <- read_csv("/Users/jarellano/Documents/GitHub/ksmcforecasttool/forecasttool2/float-people-20201103-100700-397d.csv", skip = 5)
float <- as.data.frame(float)
float <- float[!(is.na(float$`Job Title`)), ]
float <- float[ -c(4:8) ]
float <- aggregate( float[,4:370], float[,1:3], FUN = sum )


float_by_employee <- float[,-1]
rownames(float_by_employee) <- float[,1]

float_by_jobrole <- float[ -c(1) ]
float_by_jobrole['Capacity'] = 8
float_by_jobrole <- aggregate( float_by_jobrole[,3:370], float_by_jobrole[,1:2], FUN = sum )
float_by_jobrole$job_role_and_department <- paste(float_by_jobrole$`Job Title`, " - ", float_by_jobrole$`Department`)
rownames(float_by_jobrole) <- float_by_jobrole$job_role_and_department


connectwise <- read_csv("/Users/jarellano/Documents/GitHub/ksmcforecasttool/forecasttool2/TimeEntry2.csv", trim_ws = TRUE)
df = as.data.frame(connectwise)
df$Name <- paste(df$First_Name, df$Last_Name)
df <- df[ -c(1:3) ]
df <- df[c("Name", "Date_Start", "Billable_Hrs")]
df <- aggregate( df[,3], df[,1:2], FUN = sum )
df$Date_Start = as.Date(df$Date_Start,format="%m/%d/%Y")
df$Date_Start <- ymd(df$Date_Start)
df <-aggregate(df, by = list(df$Date_Start ,df$Name),  FUN=sum)
connectwise_names <- unique(df[,1])


# Define UI for application that draws a histogram
ui <- fluidPage(    
    
    # Give the page a title
    titlePanel("DDS Forecasting Tool"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            h2("Float Individual Forecasted Hours"),
            selectInput("person_id", "Choose a Person:", 
                        choices=rownames(float_by_employee)),
            
            dateRangeInput("daterange", "Date range:",
                           start  = "2020-10-15",
                           end    = "2021-01-10",
                           min    = "2020-10-15",
                           max    = "2021-01-10",
                           format = "mm/dd/yy",
                           separator = " - "),
            
            h2("Connectwise Individual Forecasted Hours"),
            selectInput("person_id2", "Choose a Person:", 
                        choices=connectwise_names),
            
            hr(),
            h2("Team Hours"),
            
            selectInput("job_id", "Choose a Job Role:", 
                        choices=rownames(float_by_jobrole)),
            
            dateRangeInput("daterange2", "Date range:",
                           start  = "2020-10-15",
                           end    = "2021-01-10",
                           min    = "2020-10-15",
                           max    = "2021-01-10",
                           format = "mm/dd/yy",
                           separator = " - "),
            hr(),
            
        ),
        
        # Create a spot for the barplot
        mainPanel(
            tabsetPanel(
                tabPanel("Float Individual Forecasted Hours", plotOutput("phonePlot")), 
                tabPanel("ConnectWise Individual Forecasted Hours", plotOutput("ConnectWisePlot")),
                #tabPanel("Float Team Hours", plotOutput("jobRolePlot"))
          
                tabPanel("Float Team Hours",
                         fluidRow(
                           plotOutput("jobRolePlot"),
                           tableOutput('table'))
                ), 
                tabPanel("Float and Connectwise Forecasted vs Actual", plotOutput("fcwPlot")))
            )
        )
        
    )


server <- function(input, output) {
  
    
    
    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlot({
        
        # Filter to person of interest
        person_data <- subset(float_by_employee, rownames(float_by_employee) == input$person_id)
        person_data <- person_data[ -c(1:2) ]
        person_transpose <- as.data.frame(t(as.matrix(person_data)))
        person_transpose$Date <- rownames(person_transpose)
        colnames(person_transpose) <- c("DailyHours","Date")
        person_transpose$Date <- dmy(person_transpose$Date)
        person_transpose['Capacity'] = 8
        
        person_transpose <- subset(person_transpose, Date >= input$daterange[1] & Date <= input$daterange[2])
        
        ggplot(person_transpose) + 
            geom_col(aes(x = Date, y = DailyHours), size = 1, color = "grey", fill = "grey") +
            geom_line(aes(x = Date, y = Capacity), size = 1.5, color="black", group = 1)
    })
    
    output$ConnectWisePlot <- renderPlot({
      connectwise_person_data <- subset(df, df$Name == input$person_id2)
      connectwise_person_data <- connectwise_person_data[ -c(1) ]
      colnames(connectwise_person_data) <- c("Date","Billable Hours")
      min_date = min(connectwise_person_data[,1])
      
      
      y = ts(connectwise_person_data$`Billable Hours`, start=c(2020, yday(min_date)), frequency=365)
      fit <- auto.arima(y)
      plot(forecast(fit, 10), xaxt="n", ylim=c(0, 12))
      print(accuracy(forecast(fit, 10)))
      a = seq(as.Date(min_date), by="weeks", length=11)
      axis(1, at = decimal_date(a), labels = format(a, "%Y %b %d"), cex.axis=0.6)
      abline(v = decimal_date(a), col='grey', lwd=0.5)
      abline(h = 8, col="black")
    })
    
    output$jobRolePlot <- renderPlot({
        
        # Filter to person of interest
        jobrole_data <- subset(float_by_jobrole, rownames(float_by_jobrole) == input$job_id)
        Capacity = jobrole_data$Capacity
        jobrole_data <- jobrole_data[ -c(1:2, 87:88) ]
        jobrole_transpose <- as.data.frame(t(as.matrix(jobrole_data)))
        jobrole_transpose$Date <- rownames(jobrole_transpose)
        colnames(jobrole_transpose) <- c("DailyHours","Date")
        jobrole_transpose$Date <- dmy(jobrole_transpose$Date)
        jobrole_transpose['Capacity'] = Capacity
        
        jobrole_transpose <- subset(jobrole_transpose, Date >= input$daterange2[1] & Date <= input$daterange2[2])
        
        ggplot(jobrole_transpose) + 
            geom_col(aes(x = Date, y = DailyHours), size = 1, color = "grey", fill = "grey") +
            geom_line(aes(x = Date, y = Capacity), size = 1.5, color="black", group = 1)
    })
    
    output$table <- output$table1 <- renderTable({
      jobrole_data <- subset(float_by_jobrole, rownames(float_by_jobrole) == input$job_id)
      Job_title = tail(names(sort(table(jobrole_data$`Job Title`))), 1)
      Department = tail(names(sort(table(jobrole_data$`Department`))), 1)
      employee_table <- subset(float_by_employee, (float_by_employee$Department == Department) & (float_by_employee$`Job Title` == Job_title))
      employee_table <- employee_table[ c(1:2) ]
      employee_table['Name'] <- rownames(employee_table)
      return(employee_table)
    })
    
    output$fcwPlot <- renderPlot({
      person_data <- subset(float_by_employee, rownames(float_by_employee) == input$person_id)
      person_data <- person_data[ -c(1:2) ]
      person_transpose <- as.data.frame(t(as.matrix(person_data)))
      person_transpose$Date <- rownames(person_transpose)
      colnames(person_transpose) <- c("DailyHours","Date")
      person_transpose$Date <- dmy(person_transpose$Date)
      person_transpose['Capacity'] = 8
      
      connectwise_person_data <- subset(df, df$Name == input$person_id2)
      connectwise_person_data <- connectwise_person_data[ -c(1) ]
      colnames(connectwise_person_data) <- c("Date","Billable Hours")
      min_date = min(connectwise_person_data[,1])
      
      newdf <- left_join(connectwise_person_data, person_transpose, by=c("Date"))
      ggplot(newdf, aes(x=Date)) + 
        geom_line(aes(y = `Billable Hours`), color = "black") + 
        geom_line(aes(y = DailyHours), color="grey") +
        ggtitle("Projected Billable Hours vs ConnectWise Actual Hours") + # for the main title
        xlab('Date') + # for the x axis label
        ylab('ConnectWise Hours (Black), Float Hours(Gray)') # for the y axis label
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)