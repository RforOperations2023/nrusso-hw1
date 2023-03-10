library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(forcats)
library("viridis")
library(rworldmap)
pldata <- read.csv("plstats.csv") 

aggregate_tbl <- pldata %>% group_by(Squad) %>% 
  summarise(Gls =sum(Gls),
            CrdY =sum(CrdY),
            CrdR =sum(CrdR),
            npxG = sum(npxG),
            .groups = 'drop')

pldata$Nation <- as.factor(pldata$Nation)
#check that ISO alpha 3 country codes are readable as factors
table(pldata$Nation)
df_countrycounts <- data.frame(table(pldata$Nation))

datajoin <- joinCountryData2Map(dF = df_countrycounts, joinCode = "ISO3",
                                nameJoinColumn = "Var1",
                                suggestForFailedCodes = TRUE,
                                mapResolution = "coarse",
                                projection = NA, verbose = FALSE)


# Convert tibble to df
pldata_df2 <- aggregate_tbl %>% as.data.frame()

# Define UI for application 
ui <- fluidPage(
  
    downloadLink('Downloaddata', 'Download'),
  
    theme = shinythemes::shinytheme("sandstone"),

    # Application title
    titlePanel("Premier League Player/Team Stats: 22-23"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h5("This menu is for the bar plot"),
          
            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        choices = c("Goals" = "Gls", 
                                    "Yellow Cards" = "CrdY", 
                                    "Non-Penalty XG" = "npxG", 
                                    "Red Cards" = "CrdR"), 
                        selected = "Gls"),
            
            
            hr(),
            
          h5("This menu is for the scatterplot"),
          
            selectInput(inputId = "a", 
                        label = "Y-axis:",
                        choices = c("Age" = "AgeFixed", 
                                    "Minutes Played" = "Min", 
                                    "Full Games" = "X90s", 
                                    "Goals" = "Gls",
                                    "Assists" = "Ast",
                                    "Expected Goals" = "xG",
                                    "Expected Goals & Assists" = "xAG"), 
                        selected = "Gls"),
            
            selectInput(inputId = "b", 
                        label = "X-axis:",
                        choices = c("Age" = "AgeFixed", 
                                    "Minutes Played" = "Min", 
                                    "Full Games" = "X90s", 
                                    "Goals" = "Gls",
                                    "Assists" = "Ast",
                                    "Expected Goals" = "xG",
                                    "Expected Goals & Assists" = "xAG"), 
                        selected = "Age"),
            
            selectInput(inputId = "z", 
                        label = "Color by:",
                        choices = c("Age" = "AgeFixed", 
                                    "Minutes Played" = "Min", 
                                    "Full Games" = "X90s", 
                                    "Goals" = "Gls",
                                    "Assists" = "Ast",
                                    "Expected Goals" = "xG",
                                    "Expected Goals & Assists" = "xAG"),
                        selected = "Age"),
            
            
            sliderInput(inputId = "size", 
                        label = "Size:", 
                        min = 0, max = 5, 
                        value = 2),

            hr(),
            
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          
           plotOutput("SquadStats"),
           
           plotOutput("PlayerStats"),
           
           plotOutput("PlayerNationality"),
           
           DT::dataTableOutput(outputId = "StatTable"),
           
           downloadButton(outputId = 'downloadData', label = "Download data",
                          class = NULL)
        )
    )
)
#server
server <- function(input, output) {

    output$SquadStats <- renderPlot({
      
      ggplot(data = pldata, aes_string(x = "Squad", y = input$y)) +
        geom_bar(stat="identity", width = .9, fill="#FF9999", colour="blue") +
        labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
             y = toTitleCase(str_replace_all(input$y, "_", " ")),
             title = "Bar Plot of Selected Statistic by Team" )+
        theme(axis.text.x = element_text(angle=65, vjust=0.6, size = 14))
      
    })
    
    output$PlayerStats <- renderPlot({
      
      ggplot(data = pldata, aes_string(x = input$b, y = input$a,
                                       color = input$z)) +
        geom_point(size = input$size) +
        ggtitle("Scatterplot of Selected Statistics - Default: Goals X Age") +
        scale_color_viridis(discrete = FALSE, option = "D")+
        scale_fill_viridis(discrete = FALSE) +
        theme_minimal() +
        theme(legend.position = "bottom", axis.text = element_text(size = 14),
              legend.text = element_text(size = 14))
#source code for color palette
#https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-
      #data-visualization/
      
      
    })
    
    output$PlayerNationality <- renderPlot({
      
      mapCountryData(mapToPlot = datajoin, nameColumnToPlot = "Freq",
                     numCats = 10,
                     xlim = NA, ylim = NA, mapRegion = "world",
                     catMethod = "logFixedWidth",
                     colourPalette = "heat", addLegend = FALSE,
                     borderCol = "Black",
                     mapTitle = "Heat Map of Premier League Player Nationality",
                     oceanCol = NA, aspect = 1,
                     missingCountryCol = "grey", add = FALSE,
                     nameColumnToHatch = "",
                     lwd = 0.5)
      
    })
    
    output$StatTable <- DT::renderDataTable(
      if(input$show_data){
        DT::datatable(data = pldata[,c( "Player", "Squad", "Gls",
                                        "Nation", "xG")], 
                      options = list(pageLength = 50), 
                      rownames = FALSE)
      })
    
    
     output$downloadData <- downloadHandler(
       filename = function() {
         paste('pldata', Sys.Date(), '.csv')
       },
       content = function(con) {
         write.csv(pldata, con)
       }
     )
}

# Run the application 
shinyApp(ui = ui, server = server)
