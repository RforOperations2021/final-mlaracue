library("shiny")
library("shinythemes")
library("plotly")
library("leaflet")
library("leaflet.extras")

ui <- fluidPage(
    
    titlePanel("NYPD Explorer"),
    title = "NYPD Explorer",
    
    themeSelector(),
    
    sidebarLayout(
        
        sidebarPanel(
            
            width = 3,
            
            dateRangeInput(
                inputId = "dates", 
                label = "Select date range:",
                start = "2021-01-01",
                end = "2021-01-25",
                min = "2019-01-01",
                max = "2021-01-25"
            ),
            
            radioButtons(
                inputId = "by", 
                label = "Analyze by:", 
                choices = c("Borough", "Location"), 
                selected = "Borough", 
                inline = TRUE
            ),
            
            uiOutput("byoptions"),
            
            selectInput(
                inputId = "offense",
                label = "Level of offense:",
                choices = c("All", "Misdemeanor", "Violation", "Felony"),
                selected = "All",
                multiple = FALSE
            ),
            
            numericInput(
                inputId = "size",
                label = "Sample size:",
                value = 100,
                min = 0,
                max = 10000,
                step = 100
            )
        ),
        
        mainPanel(
            width = 9,
            
            tabsetPanel(
                selected = "Plots",
                
                tabPanel(
                    
                    title = "Plots",
                    
                    plotlyOutput(outputId = "timeseries", height = "80%"),
                    
                    fluidRow(
                        
                        column(
                            width = 8,
                            leafletOutput("map", height = "40%")
                        ),
                        
                        column(
                            width = 4,
                            
                            radioButtons(
                                inputId = "perspective", 
                                label = "Select one:", 
                                choices = c("All", "Level of Offense"), 
                                selected = "All", 
                                inline = TRUE
                            ),
                            
                            plotlyOutput(outputId = "barplot"),
                            
                            plotlyOutput(outputId = "lineplot")
                        )
                    )
                ), 
                
                tabPanel(title = "Data", tableOutput("table"))
            )
        )
    )
)

server <- function(input, output, session) {
    
    output$byoptions <- renderUI({
        
        if(input$by == "Borough"){
            
            selectInput(
                inputId = "borough",
                label = "Select a Borough:",
                choices = c("Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island"),
                multiple = TRUE,
                selectize = TRUE
            )
            
        } else {
            
            column(
                width = 12,
                
                numericInput(
                    inputId = "lat", 
                    label = "Latitude:",
                    value = 40.785091
                ),
                
                numericInput(
                    inputId = "lon", 
                    label = "Longitude:",
                    value = -73.968285
                ),
                
                numericInput(
                    inputId = "radius", 
                    label = "Radius:",
                    value = 10
                )
            )
        }
    })
}

# run the application
shinyApp(ui = ui, server = server)