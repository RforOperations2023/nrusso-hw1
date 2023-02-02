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
library(DT)
library(stringr)
library(dplyr)
library(tools)
pldata <- read.csv("plstats.csv") #I realized it needed to be in the same file as the app

aggregate_tbl <- pldata %>% group_by(Squad) %>% 
  summarise(Gls =sum(Gls),
            CrdY =sum(CrdY),
            CrdR =sum(CrdR),
            npxG = sum(npxG),
            .groups = 'drop')

# Convert tibble to df
pldata_df2 <- aggregate_tbl %>% as.data.frame()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Premier League Player Stats: 22-23"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        choices = c("Goals" = "Gls", 
                                    "Yellow Cards" = "CrdY", 
                                    "Non-Penalty XG" = "npxG", 
                                    "Red Cards" = "CrdR"), 
                        selected = "Gls"),
            
            selectInput(inputId = "x", 
                        label = "X-axis:",
                        choices = c("Team" = "Squad"), 
                        selected = "Squad"),
            
            
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("SquadStats")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$SquadStats <- renderPlot({
        # generate bins based on input$bins from ui.R

        # draw the histogram with the specified number of bins
      ggplot(data = pldata, aes_string(x = input$x, y = input$y)) +
        geom_bar(stat="identity") +
        labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
             y = toTitleCase(str_replace_all(input$y, "_", " ")))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
