library(tidyverse)
library(shiny)
library(shinymanager)
#library(shinyjs)
library(dplyr)
library(data.table)
library(lubridate)
library(xts)
library(forecast)
#library(prophet)
library(tidyr)
library(Metrics)
library(rsconnect)

float <- read_csv("float.csv")
float_by_jobrole <- read_csv("float_by_jobrole.csv")
connectwise <- read_csv("connectwise.csv")

credentials <- data.frame(
    user = c("jarellano", "awheeler", "mschwarz"), # mandatory
    password = c("ksmc1234", "728hb43m", "6uten4zu") # mandatory
    #start = c("2019-04-15"), # optinal (all others)
    #expire = c(NA, "2019-12-31"),
    #admin = c(FALSE, TRUE),
    #comment = "Simple and secure authentification mechanism 
  #for single ‘Shiny’ applications.",
    #stringsAsFactors = FALSE
)

#user_base <- data.frame(
    #user = c("jarellano", "awheeler", "mschwarz"),
    #password = c("pass1", "pass2", "pass3"), 
    #permissions = c("admin", "standard", "standard"),
    #name = c("Juan", "Amrutha", "Michael"),
    #stringsAsFactors = FALSE
#)

# Define UI for application that draws a histogram
ui <- fluidPage(    
    
    # must turn shinyjs on
    #shinyjs::useShinyjs(),
    # add logout button UI 
    # add login panel UI function
    #shinyauthr::loginUI(id = "login"),

    
    # Give the page a title
    titlePanel("DDS Forecasting Tool"),
    
    # Generate a row with a sidebar
    sidebarLayout( 
        # Define the sidebar with one input
        sidebarPanel(
        
            h2("Float Individual Forecasted Hours"),
            selectInput("person_id", "Choose a Person:", 
                        choices= unique(float$Name)),
            
            dateRangeInput("daterange", "Date range:",
                           start  = "2020-01-01",
                           end    = "2021-01-10",
                           min    = "2020-01-01",
                           max    = "2021-01-10",
                           format = "mm/dd/yy",
                           separator = " - "),
            
            h2("Connectwise Individual Forecasted Hours"),
            selectInput("person_id2", "Choose a Person:", 
                        choices= unique(connectwise$Name)),
            
            hr(),
            h2("Team Hours"),
            
            selectInput("job_id", "Choose a Job Role:", 
                        choices= unique(float_by_jobrole$`Job Title`)),
            
            dateRangeInput("daterange2", "Date range:",
                           start  = "2020-01-01",
                           end    = "2021-01-10",
                           min    = "2020-01-01",
                           max    = "2021-01-10",
                           format = "mm/dd/yy",
                           separator = " - "),
            hr(),
            
            h2("Float and ConnectWise Compared Hours - Individual"),
            selectInput("person_id3", "Choose a Person:", 
                        choices= unique(float$Name)),
            
            
            dateRangeInput("daterange3", "Date range:",
                           start  = "2020-01-01",
                           end    = "2021-01-10",
                           min    = "2020-01-01",
                           max    = "2021-01-10",
                           format = "mm/dd/yy",
                           separator = " - "),
            
            hr(),
            
            h2("Float and ConnectWise Compared Hours - Team"),
            selectInput("team_id", "Choose a Team:", 
                        choices= unique(float$Department)),
            
            dateRangeInput("daterange4", "Date range:",
                           start  = "2020-01-01",
                           end    = "2021-01-10",
                           min    = "2020-01-01",
                           max    = "2021-01-10",
                           format = "mm/dd/yy",
                           separator = " - ")
            
        ),
        
        # Create a spot for the barplot
        mainPanel(
            tabsetPanel(
                tabPanel("Turnover Calculator", htmlOutput("googleSheet")),
                tabPanel("Float Individual Forecasted Hours", plotOutput("phonePlot")), 
                tabPanel("ConnectWise Individual Forecasted Hours", plotOutput("ConnectWisePlot")),
                tabPanel("Float Team Hours",
                         fluidRow(
                             plotOutput("jobRolePlot"),
                             tableOutput('table'))
                ), 
                tabPanel("Float and Connectwise Forecasted vs Actual - Individual", 
                         fluidRow(
                             plotOutput("fcwPlot"),
                             tableOutput('table2'))),
            
                tabPanel("Float and Connectwise Forecasted vs Actual - Team", 
                         fluidRow(
                             plotOutput("fcwPlot2"),
                             tableOutput('table3'))))
        )
    )
    
)

ui <- secure_app(ui)

server <- function(input, output) {
    # call the server part
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    
    # call the logout module with reactive trigger to hide/show
    #logout_init <- callModule(shinyauthr::logout, 
                              #id = "logout", 
                              #active = reactive(credentials()$user_auth))
    
    # call login module supplying data frame, user and password cols
    # and reactive trigger
    #credentials <- callModule(shinyauthr::login, 
                              #id = "login", 
                              #data = user_base,
                              #user_col = user,
                              #pwd_col = password,
                              #log_out = reactive(logout_init()))
    
    # pulls out the user information returned from login module
    #user_data <- reactive({credentials()$info})
    
        
    output$googleSheet <- renderUI({
        #req(credentials()$user_auth)
            tags$iframe(id = "googleSheet",
                        src = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQeNg9nC_d-wNDFelmGwDuh2BAxxzkEvD3IA3VZmMc7AJ0Q-DGdx-bDlrx5KK87ZV9e558LNSyy5xHd/pubhtml?gid=0&amp;single=true&amp;widget=true&amp;headers=false",
                        width = 1024,
                        height = 768,
                        frameborder = 0,
                        marginheight = 0)
        })
    
    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlot({
        #req(credentials()$user_auth)
        # Filter to person of interest
        person_data <- subset(float, float$Name == input$person_id)
        person_data['Capacity'] = 8
        person_data <- subset(person_data, Date >= input$daterange[1] & Date <= input$daterange[2])
        
        ggplot(person_data) + 
            geom_col(aes(x = Date, y = float_Billable_Hours), size = 1, color = "orange", fill = "orange") +
            geom_line(aes(x = Date, y = Capacity), size = 1.5, color="blue", group = 1) +
            ggtitle("Float Projected Hours") + # for the main title
            xlab('Date') + # for the x axis label
            ylab('Number of Billable Hours')
    })
    
    output$ConnectWisePlot <- renderPlot({
        #req(credentials()$user_auth)
        connectwise_person_data <- subset(connectwise, connectwise$Name == input$person_id2)
        connectwise_person_data <- connectwise_person_data[ -c(1) ]
        colnames(connectwise_person_data) <- c("Date","Billable Hours")
        
        #colnames(connectwise_person_data) <- c('ds', 'y')
        #m <- prophet(connectwise_person_data)
        #future <- make_future_dataframe(m, periods = 365)
        #forecast <- predict(m, future)
        #plot(m, forecast)
        
        min_date = min(connectwise_person_data$Date)
        y = ts(connectwise_person_data$`Billable Hours`, start=c(2020, yday(min_date)), frequency=365)
        fit <- auto.arima(y)
        plot(forecast(fit, 10), xaxt="n", ylim=c(0, 12))
        a = seq(as.Date(min_date), by="weeks", length=11)
        axis(1, at = decimal_date(a), labels = format(a, "%Y %b %d"), cex.axis=0.6)
        abline(v = decimal_date(a), col='orange', lwd=0.5)
        abline(h = 8, col="blue")
        
    })
    
    output$jobRolePlot <- renderPlot({
        #req(credentials()$user_auth)
        # Filter to person of interest
        
        jobrole_data <- subset(float_by_jobrole, float_by_jobrole$`Job Title` == input$job_id)
        jobrole_data <- subset(jobrole_data, Date >= input$daterange2[1] & Date <= input$daterange2[2])
        
        ggplot(jobrole_data) + 
            geom_col(aes(x = Date, y = float_Billable_Hours), size = 1, color = "orange", fill = "orange") +
            geom_line(aes(x = Date, y = Capacity), size = 1.5, color="blue", group = 1)
    })
    
    
    output$table <- renderTable({
        #req(credentials()$user_auth)
        employee_table <- float
        employee_table$`Job Title` <- paste(employee_table$`Job Title`, " - ", employee_table$`Department`)
        employee_table <- subset(employee_table, employee_table$`Job Title` == input$job_id)
        employee_table <- employee_table[ c(1:2) ]
        employee_table <- unique(employee_table)
        return(employee_table)
    })
    
    
    output$fcwPlot <- renderPlot({
        #req(credentials()$user_auth)
        person_data <- subset(float, float$Name == input$person_id3)
        person_data <- person_data[ -c(1:3) ]
        person_data['Capacity'] = 8
        
        connectwise_person_data <- subset(connectwise, connectwise$Name == input$person_id3)
        connectwise_person_data <- connectwise_person_data[ -c(1) ]
        colnames(connectwise_person_data) <- c("Date","Connectwise Billable Hours")
        min_date = min(connectwise_person_data$Date)
        
        newdf <- right_join(connectwise_person_data, person_data, by=c("Date"))
        newdf <- subset(newdf, Date >= min_date)
        newdf$Date <- as.POSIXlt(newdf$Date,format="%Y-%m-%d")
        newdf$`Connectwise Billable Hours` <-  na.interp(newdf$`Connectwise Billable Hours`)
        newdf$Date <- ymd(newdf$Date)
        
        newdf <- subset(newdf, Date >= input$daterange3[1] & Date <= input$daterange3[2])
        
        ggplot(newdf, aes(x=Date)) + 
            geom_line(aes(y = `Connectwise Billable Hours`), color = "blue") + 
            geom_line(aes(y = float_Billable_Hours), color="orange") +
            ggtitle("Projected Billable Hours vs ConnectWise Actual Hours") + # for the main title
            xlab('Date') + # for the x axis label
            ylab('ConnectWise Hours (Blue), Float Hours(Orange)')
    })
    
    output$table2 <- renderTable({
        #req(credentials()$user_auth)
        
        person_data <- subset(float, float$Name == input$person_id3)
        person_data <- person_data[ -c(1:3) ]
        
        connectwise_person_data <- subset(connectwise, connectwise$Name == input$person_id3)
        connectwise_person_data <- connectwise_person_data[ -c(1) ]
        colnames(connectwise_person_data) <- c("Date","Connectwise Billable Hours")
        
        connectwise_person_data$Date <- as.Date(connectwise_person_data$Date)
        person_data$Date <- as.Date(person_data$Date)
        
        min_date = as.Date(min(connectwise_person_data$Date))
        max_date = as.Date(max(connectwise_person_data$Date))
        
        newdf <- right_join(connectwise_person_data, person_data, by=c("Date"))
        newdf <- subset(newdf, Date >= input$daterange3[1] & Date <= input$daterange3[2])
        newdf <- subset(newdf, Date >= min_date & Date <= max_date)
        newdf[is.na(newdf)] = 0
        MeanSquaredError <- mse(newdf$"Connectwise Billable Hours", newdf$float_Billable_Hours)
        RootMeanSquaredError <- rmse(newdf$"Connectwise Billable Hours", newdf$float_Billable_Hours)
        
        metric <- c('Mean Squared Error','Root Mean Squared Error')
        value <- c(MeanSquaredError, RootMeanSquaredError)
        error_table <- data.frame(metric, value)
        
        return(error_table)
    })
    
    output$fcwPlot2 <- renderPlot({
        #req(credentials()$user_auth)
        team_data <- subset(float, float$Department == input$team_id)
        team_data <- team_data[ -c(2) ]
        team_data['Capacity'] = 8
        connectwise_team_data <- connectwise[(connectwise$Name %in% team_data$Name), ]
        newdf <- inner_join(connectwise_team_data, team_data, by=c("Date", "Name"))
        newdf$Date <- as.POSIXlt(newdf$Date,format="%Y-%m-%d")
        newdf <- newdf[ -c(4) ]
        
        #newdf <- newdf %>%
        #group_by(Name, Date) %>%
        #mutate(x <- na.interp(x))    
        #proof <- newdf %>%
        #group_by(Name, Date) %>%
        #na.interp(x)
        #proof <- newdf %>%
        #group_by(Name) %>%
        #mutate(ValueInterp = na.interp(x))
        #min_date = min(connectwise_person_data$Date)
        
        newdf <- newdf %>% 
            group_by(Date) %>%
            summarise(across(c(x, float_Billable_Hours, Capacity), sum))
        
        newdf$Date <- ymd(newdf$Date)
        
        newdf <- subset(newdf, Date >= input$daterange4[1] & Date <= input$daterange4[2])
        
        ggplot(newdf, aes(x=Date)) + 
            geom_line(aes(y = x), color = "blue") + 
            geom_line(aes(y = float_Billable_Hours), color="orange") +
            ggtitle("Projected Billable Hours vs ConnectWise Actual Hours") + # for the main title
            xlab('Date') + # for the x axis label
            ylab('ConnectWise Hours (Blue), Float Hours(Orange)')
    })
    
    output$table3 <- renderTable({
        #req(credentials()$user_auth)
        team_data <- subset(float, float$Department == input$team_id)
        team_data <- team_data[ -c(2) ]
        team_data['Capacity'] = 8
        connectwise_team_data <- connectwise[(connectwise$Name %in% team_data$Name), ]
        newdf <- inner_join(connectwise_team_data, team_data, by=c("Date", "Name"))
        newdf$Date <- as.POSIXlt(newdf$Date,format="%Y-%m-%d")
        newdf <- newdf[ -c(4) ]
        newdf <- newdf %>% 
            group_by(Date) %>%
            summarise(across(c(x, float_Billable_Hours, Capacity), sum))
        
        newdf$Date <- ymd(newdf$Date)
        min_date = as.Date(min(newdf$Date))
        max_date = as.Date(max(newdf$Date))
        
        newdf <- subset(newdf, Date >= input$daterange4[1] & Date <= input$daterange4[2])
        newdf <- subset(newdf, Date >= min_date & Date <= max_date)
        MeanSquaredError <- mse(newdf$x, newdf$float_Billable_Hours)
        RootMeanSquaredError <- rmse(newdf$x, newdf$float_Billable_Hours)
        
        metric <- c('Mean Squared Error','Root Mean Squared Error')
        value <- c(MeanSquaredError, RootMeanSquaredError)
        error_table <- data.frame(metric, value)
        
        return(error_table)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)