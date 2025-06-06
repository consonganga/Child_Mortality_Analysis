library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(shiny)
library(shinythemes)

# Loading the data set

Child_Mortality_data_set_Cleaned <- read_excel("C:/Users/admin/Desktop/Masters-MSC/APHREA/Health Data Sets/Child Mortality data set- Cleaned.xlsx")

# Shiny App

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Child Mortality: Prevalence by Cause of Death"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Continent", "Select Continent:",
                  choices = unique(Child_Mortality_data_set_Cleaned$Continent),
                  multiple = TRUE),
      
      selectInput("Country", "Select Country:",
                  choices = NULL, multiple = TRUE),
      
      selectInput("Year", "Select Year:",
                  choices = unique(Child_Mortality_data_set_Cleaned$Year),
                  multiple = TRUE),
      
      selectInput("Age_Group", "Select Age Group:",
                  choices = unique(Child_Mortality_data_set_Cleaned$Age_Group),
                  multiple = TRUE),
      
      selectizeInput("Cause_of_Death", "Select Cause of Death:",
                     choices = c("All"), multiple = TRUE)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Cache all unique causes of death
  all_causes <- unique(Child_Mortality_data_set_Cleaned$Cause_of_Death)
  
  # Initialize cause of death dropdown with "All"
  updateSelectizeInput(session, "Cause_of_Death",
                       choices = c("All", all_causes),
                       selected = all_causes,
                       server = TRUE)
  
  # Observe Cause_of_Death input and apply "Select All" behavior
  observeEvent(input$Cause_of_Death, {
    if (!is.null(input$Cause_of_Death)) {
      if ("All" %in% input$Cause_of_Death &&
          !setequal(input$Cause_of_Death, all_causes)) {
        isolate({
          updateSelectizeInput(session, "Cause_of_Death",
                               selected = all_causes)
        })
      }
    }
  }, ignoreInit = TRUE)
  
  # Update Country choices based on selected Continent(s)
  observeEvent(input$Continent, {
    req(input$Continent)
    
    available_countries <- Child_Mortality_data_set_Cleaned %>%
      filter(Continent %in% input$Continent) %>%
      distinct(Country) %>%
      arrange(Country) %>%
      pull(Country)
    
    updateSelectInput(session, "Country",
                      choices = available_countries,
                      selected = available_countries)
  })
  
  # Plot output
  output$plot <- renderPlot({
    req(input$Continent, input$Country, input$Year, input$Age_Group, input$Cause_of_Death)
    
    filtered_data <- Child_Mortality_data_set_Cleaned %>%
      filter(
        Continent %in% input$Continent,
        Country %in% input$Country,
        Year %in% input$Year,
        Age_Group %in% input$Age_Group,
        Cause_of_Death %in% input$Cause_of_Death
      )
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    ggplot(filtered_data, aes(x = reorder(Cause_of_Death, -Prevalence), y = Prevalence)) +
      geom_bar(stat = "identity", fill = "tomato") +
      labs(title = "Prevalence by Cause of Death",
           x = "Cause of Death", y = "Prevalence") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)
