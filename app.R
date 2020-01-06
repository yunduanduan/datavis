#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(ggthemes)
library(extracat)
library(forcats)

ges <- read_csv("ges_cleaned.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring Degree Programs within a Specific Area"),

    # Add a select box
    selectInput(inputId = "category", 
        label = "Choose a program category", 
                    choices = unique(ges$area)),
    
    # Add a select box
    selectInput(inputId = "metric", 
                label = "Choose a metric", 
                choices = c("Full-time Employment Rate (%)", "Average Gross Salary (S$)")),


    # Show a plot of the generated distribution
    mainPanel(
        p("Hover over a point to view specific program title."),
        
        plotlyOutput("dotPlot")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dotPlot <- renderPlotly({
        if (input$metric == "Average Gross Salary (S$)") {
            p <- ges %>%
                filter(area == input$category, year == 2017) %>%
                ggplot(aes(x=fct_reorder(degree, gross_monthly_mean, .desc = FALSE),  text = degree,
                           y=gross_monthly_mean, color = university)) +
                geom_point() +
                xlab("Degree") +
                ylab(input$metric) +
                theme(axis.text.y = element_blank()) +
                coord_flip() 
            ggplotly(p, tooltip = c('y', 'text'))
                       
        }
        
        else {
            p <- ges %>%
                filter(area == input$category, year == 2017) %>%
                ggplot(aes(x=fct_reorder(degree, employment_rate_ft_perm, .desc = FALSE), 
                           y=employment_rate_ft_perm, text = degree, color = university)) +
                geom_point() +
                xlab("Degree") +
                ylab(input$metric) +
                theme(axis.text.y = element_blank()) +
                coord_flip() 
            ggplotly(p, tooltip = c('text', 'y'))
        }

    })
}

# Run the application 
shinyApp(ui = ui, server = server)











