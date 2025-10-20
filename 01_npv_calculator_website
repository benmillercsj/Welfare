# Clear global environment

rm(list = ls())

# Load packages and set working directories

library(ggplot2)
library(stringr)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(janitor)
library(dataCompareR)
library(grid)
library(sf)
library(ggbreak)
library(glue)
library(purrr)
library(openxlsx)
library(shiny)
library(ggplot2)
library(DT) 
library(rsconnect)

# Load in functions

'%!in%' = Negate("%in%")

# Import data

stacked_by_year <- read_excel("data/Stacked by year.xlsx")
stacked_totals <- read_excel("data/Stacked totals.xlsx")

ui <- fluidPage(
  titlePanel("Lifetime Fiscal Cost Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      # Age filter
      selectInput("start_age", "Age of Entry:",
                  choices = unique(stacked_totals$start_age), selected = 25),
      
      selectInput("uc_standard_input", "Annual Standard UC (£s):",
                  choices = unique(stacked_totals$uc_standard_input), selected = 4802),
      
      selectInput("lcwra_input", "Annual LCWRA (£s):",
                  choices = unique(stacked_totals$lcwra_input), selected = 5079),
      
      selectInput("housing_input", "Annual Housing (£s):",
                  choices = unique(stacked_totals$housing_input), selected = 6968),
      
      selectInput("pip_input", "Annual PIP (£s):",
                  choices = unique(stacked_totals$pip_input), selected = 7166),
      
      selectInput("probabilities", "Probabilities (Retention Rates Years 1, 2, 3, 4+):",
                  choices = unique(stacked_totals$probabilities), selected = "0.89, 0.89, 0.89, 0.89"),
      
      selectInput("counterfactual_wage", "Counterfactual Salary:",
                  choices = unique(stacked_totals$counterfactual_wage), selected = "Minimum wage"),
      
      selectInput("discount_rate", "Discount Rate:",
                  choices = unique(stacked_totals$discount_rate), selected = 0.035),
      
      selectInput("inflation_rate", "Inflation Rate:",
                  choices = unique(stacked_totals$inflation_rate), selected = 0.02),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Total Lifetime Cost", DTOutput("total_cost")),
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Costs by Year", DTOutput("cost_by_year"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Total Cost
  
  output$total_cost <- renderDT({
    datatable(stacked_totals %>% 
                filter(start_age == input$start_age,
                       uc_standard_input == input$uc_standard_input,
                       lcwra_input == input$lcwra_input,
                       housing_input == input$housing_input,
                       pip_input == input$pip_input,
                       probabilities == input$probabilities,
                       counterfactual_wage == input$counterfactual_wage,
                       discount_rate == input$discount_rate,
                       inflation_rate == input$inflation_rate) %>% 
                transmute(`Total Lifetime Cost Estimate` = paste0("£", format(cumulative_total, big.mark = ",", nsmall = 0))),
              options = list(pageLength = 5))
  })
  
  # Plot
  output$plot <- renderPlot({
    ggplot(stacked_by_year %>% 
             filter(start_age == input$start_age,
                    uc_standard_input == input$uc_standard_input,
                    lcwra_input == input$lcwra_input,
                    housing_input == input$housing_input,
                    pip_input == input$pip_input,
                    probabilities == input$probabilities,
                    counterfactual_wage == input$counterfactual_wage,
                    discount_rate == input$discount_rate,
                    inflation_rate == input$inflation_rate) %>% 
             select(age,
                    `Cumulative Welfare` = cumulative_benefits,
                    `Cumulative Tax Loss` = cumulative_tax,
                    `Total` = cumulative_total) %>% 
             pivot_longer(cols = -age),
           aes(x = age, y = value, group = name, colour = name)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(breaks = pretty_breaks(n = 6)) +
      labs(x = "", y = "") 
  })
  
  # Data table
  output$cost_by_year <- renderDT({
    datatable(stacked_by_year %>% 
                filter(start_age == input$start_age,
                       uc_standard_input == input$uc_standard_input,
                       lcwra_input == input$lcwra_input,
                       housing_input == input$housing_input,
                       pip_input == input$pip_input,
                       probabilities == input$probabilities,
                       counterfactual_wage == input$counterfactual_wage,
                       discount_rate == input$discount_rate,
                       inflation_rate == input$inflation_rate) %>% 
                select(age, probability,
                       `Cumulative Welfare` = cumulative_benefits,
                       `Cumulative Tax Loss` = cumulative_tax,
                       `Total` = cumulative_total)
              , options = list(pageLength = 42)) 
  })
}

shinyApp(ui, server)

