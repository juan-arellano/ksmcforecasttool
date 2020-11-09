library(tidyverse)
library(dplyr)
library(data.table)
library(fullcalendar)
library(rsconnect)
library(shiny)
library(readxl)

PTO_form <- read_excel("PTO Form.xlsx")
PTO_form <- PTO_form[, -c(1:4, 8)] # delete columns 1 through 4

PTO_form$Color[PTO_form$Name == "Ying Yu"] <- "red"
PTO_form$Color[PTO_form$Name == "Hannah King"] <- "blue"
PTO_form$Color[PTO_form$Name == "Alexa Kovacs"] <- "yellow"
PTO_form$Color[PTO_form$Name == "Juan Arellano"] <- "green"
PTO_form$Color[PTO_form$Name == "Amrutha Wheeler"] <- "orange"

colnames(PTO_form) <- c("Name","start_Date", "end_Date", "Color")


PTO <- read_csv("PTO.csv", trim_ws = TRUE)
PTO = as.data.frame(PTO)
PTO$Name <- paste(PTO$first_name, PTO$last_name)
PTO <- PTO %>% filter(
    Name == "Ying Yu" | Name == "Juan Arellano Marquez" | Name == "Hannah King" | Name == "Alexa Kovacs" | Name == "Amrutha Wheeler"
)

PTO$annualpto[PTO$Name == "Hannah King"] <- 96
PTO$annualpto[PTO$Name == "Alexa Kovacs"] <- 126
PTO$annualpto[PTO$Name == "Juan Arellano Marquez"] <- 96
PTO$annualpto[PTO$Name == "Amrutha Wheeler"] <- 175

PTO <- PTO[, -c(1:3)] # delete columns 1 through 3

table <- as.data.table(PTO)

data = data.frame(title = paste("PTO - ", PTO_form$Name),
                  start = PTO_form$start_Date,
                  end = PTO_form$end_Date + 1,
                  color = PTO_form$Color)

shinyApp(

  ui <- fluidPage(
      
      titlePanel("The Miner League PTO"),
      
      fluidRow(
          
          column(8,
                 fullcalendarOutput("calendar")
          ),
          
          column(4,
                 dataTableOutput('table')
          )
      )
  ),
    
    server = function(input, output) {
        output$table <- renderDataTable(table)
        
        output$calendar <- renderFullcalendar({
            fullcalendar(data)
        })
        
    }
)


#shinyApp(ui = ui, server = server)