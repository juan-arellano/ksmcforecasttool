library(tidyverse)
library(shiny)
library(dplyr)
library(data.table)
library(lubridate)
library(xts)
library(forecast)

float <- read_csv("/Users/jarellano/Documents/GitHub/ksmcforecasttool/forecasttool2/float-people-20201022-124011-84d.csv", skip = 5)
float <- as.data.frame(float)
float <- float[!(is.na(float$`Job Title`)), ]
float <- float[ -c(4:8) ]
float <- aggregate( float[,4:87], float[,1:3], FUN = sum )

float_by_employee <- float[,-1]
rownames(float_by_employee) <- float[,1]

float_by_jobrole <- float[ -c(1) ]
float_by_jobrole['Capacity'] = 8
float_by_jobrole <- aggregate( float_by_jobrole[,3:87], float_by_jobrole[,1:2], FUN = sum )
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
            
            helpText("Please select how many months you would like to forecast ahead by."),
            numericInput("ahead", "Months to Forecast Ahead:", 12),
            
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
                tabPanel("Team Hours", plotOutput("jobRolePlot"))
            )
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
            geom_col(aes(x = Date, y = DailyHours), size = 1, color = "darkblue", fill = "darkblue") +
            geom_line(aes(x = Date, y = Capacity), size = 1.5, color="red", group = 1)
    })
    
    output$ConnectWisePlot <- renderPlot({
      connectwise_person_data <- subset(df, df$Name == input$person_id2)
      connectwise_person_data <- connectwise_person_data[ -c(1) ]
      colnames(connectwise_person_data) <- c("Date","Billable Hours")
      min_date = min(connectwise_person_data[,1])
      y = ts(connectwise_person_data$`Billable Hours`, start=c(2020, yday(min_date)), frequency=365)
      fit <- ets(y)
      plot(forecast(fit, h=input$ahead))
      abline(h = 8, col="red")
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
            geom_col(aes(x = Date, y = DailyHours), size = 1, color = "darkblue", fill = "darkblue") +
            geom_line(aes(x = Date, y = Capacity), size = 1.5, color="red", group = 1)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)