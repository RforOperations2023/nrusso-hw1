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
    titlePanel("Premier League Player/Team Stats: 22-23"),

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
            
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("SquadStats"),
           
           DT::dataTableOutput(outputId = "StatTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$SquadStats <- renderPlot({
        # generate bins based on input$bins from ui.R

        # draw the histogram with the specified number of bins
      ggplot(data = pldata, aes_string(x = input$x, y = input$y)) +
        geom_bar(stat="identity", width = .4, fill = "blue") +
        labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
             y = toTitleCase(str_replace_all(input$y, "_", " "))) +
        theme(axis.text.x = element_text(angle=65, vjust=0.6))
      
    })
    
    output$StatTable <- DT::renderDataTable(
      if(input$show_data){
        DT::datatable(data = pldata[,c( "Player", "Squad", "Gls")], 
                      options = list(pageLength = 50), 
                      rownames = FALSE)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
