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
library(scales)

# Load in functions

'%!in%' = Negate("%in%")
pound_format <- label_number(prefix = "£", big.mark = ",", accuracy = 1)

# Import data

stacked_by_year <- read_excel("data/Stacked by year.xlsx")
stacked_totals <- read_excel("data/Stacked totals.xlsx")

ui <- fluidPage(
  titlePanel("Expected Lifetime Fiscal Cost Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      # Age filter
      selectInput("start_age", "Age of Entry:",
                  choices = unique(stacked_totals$start_age), selected = 25),
      
      selectInput("lcw_input", "LCW/LCWRA:",
                  choices = unique(stacked_totals$lcw_input), selected = "LCWRA"),
      
      selectInput("housing_input", "Annual Housing:",
                  choices = unique(stacked_totals$housing_input), selected = "Average housing allowance (£5,048)"),
      
      selectInput("pip_input", "Annual PIP:",
                  choices = unique(stacked_totals$pip_input), selected = "Basic PIP (£3,843)"),
      
      selectInput("probabilities", "Probabilities (Retention Rates Years 1, 2, 3, 4+):",
                  choices = unique(stacked_totals$probabilities), selected = "0.92, 0.96, 0.98, 0.99"),
      
      selectInput("counterfactual_wage", "Counterfactual Salary:",
                  choices = unique(stacked_totals$counterfactual_wage), selected = "Minimum wage"),
      
      selectInput("entitlement_in_work", "In-work entitlement:",
                  choices = unique(stacked_totals$entitlement_in_work), selected = "No in-work entitlement"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Expected Lifetime Cost", DTOutput("total_cost")),
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
                       lcw_input == input$lcw_input,
                       housing_input == input$housing_input,
                       pip_input == input$pip_input,
                       probabilities == input$probabilities,
                       counterfactual_wage == input$counterfactual_wage,
                       entitlement_in_work == input$entitlement_in_work) %>% 
                transmute(`Expected Lifetime Cost` = paste0("£", format(cumulative_total, big.mark = ",", nsmall = 0))),
              options = list(pageLength = 1))
  })
  
  # Plot
  output$plot <- renderPlot({
    ggplot(stacked_by_year %>% 
             filter(start_age == input$start_age,
                    lcw_input == input$lcw_input,
                    housing_input == input$housing_input,
                    pip_input == input$pip_input,
                    probabilities == input$probabilities,
                    counterfactual_wage == input$counterfactual_wage,
                    entitlement_in_work == input$entitlement_in_work) %>% 
             select(age,
                    `Cumulative Welfare` = cumulative_benefits,
                    `Cumulative Tax Loss` = cumulative_tax,
                    `Cumulative Total` = cumulative_total) %>% 
             pivot_longer(cols = -age) %>% 
             mutate(name = factor(name, levels = c("Cumulative Tax Loss", "Cumulative Welfare", "Cumulative Total"))),
           aes(x = age, y = value, group = name, colour = name)) +
      geom_point() +
      geom_line() +
      scale_y_continuous(breaks = pretty_breaks(n = 6),
                         labels = label_comma(prefix = "£"),
                         expand = c(0,0)) +
      scale_x_continuous(breaks = pretty_breaks(n = 6), expand = c(0,0)) +
      labs(x = "", y = "") +
      theme_minimal(base_size = 32/.pt, base_family = "sans") %+replace%
      theme(
        text = element_text(colour = "#829298"),
        plot.title = element_text(size = rel(1.5), colour = "#829298", hjust = 0),
        plot.subtitle = element_text(size = rel(1), colour = "#829298"),
        plot.caption = element_text(size = rel(0.8), colour = "#829298"),
        axis.title = element_text(colour = "#829298"),
        axis.text = element_text(colour = "#829298"),
        axis.line = element_line(colour = "#e7ecf0"),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_line(colour = "#e7ecf0"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF", colour = NA),
        plot.background = element_rect(fill = "#FFFFFF", colour = NA),
        legend.position = "bottom",
        #legend.margin = margin(t = -20, unit = "pt"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#FFFFFF", colour = NA)
      ) +
      scale_colour_manual(values = c("#829298", "#e0bbbc", "#a12241"))
  })
  
  # Data table
  output$cost_by_year <- renderDT({
    datatable(stacked_by_year %>% 
                filter(start_age == input$start_age,
                       lcw_input == input$lcw_input,
                       housing_input == input$housing_input,
                       pip_input == input$pip_input,
                       probabilities == input$probabilities,
                       counterfactual_wage == input$counterfactual_wage,
                       entitlement_in_work == input$entitlement_in_work) %>% 
                transmute(Age = age,
                          `Retention Probability` = str_c(round(100*probability, 2), "%"),
                          `Cumulative Welfare` = pound_format(cumulative_benefits),
                          `Cumulative Tax Loss` = pound_format(cumulative_tax),
                          `Cumulative In-Work Welfare` = pound_format(cumulative_in_work_benefit),
                          `Cumulative Tax Loss Net In-Work Welfare` = pound_format(cumulative_tax_loss_net_in_work_benefit),
                          `Cumulative Total` = pound_format(cumulative_total))
              , options = list(pageLength = 42))
  })
}

shinyApp(ui, server)

