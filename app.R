library("shiny")
library("shinythemes")
library("plotly")
library("leaflet")
library("leaflet.extras")

library("readr")
library("tidyr")
library("dplyr")
library("lubridate")

df <- read_csv(file = "dummy-data.csv", 
               col_types = cols(
                   .default = col_character()
               )
)

clean_data <- function(df){
    df_cleaned <- df %>% 
        mutate_at(.vars = c("boro_nm", "law_cat_cd", "ofns_desc"),
                  .funs = str_to_title) %>% 
        mutate_at(.vars = c("latitude", "longitude"),
                  .funs = as.numeric) %>% 
        mutate(id = 1:nrow(.),
               cmplnt_fr_dt = str_replace_all(cmplnt_fr_dt, 
                                              pattern = "T00:00:00.000",
                                              replacement = ""),
               datetime = paste(cmplnt_fr_dt, " ", cmplnt_fr_tm),
               datetime = as_datetime(datetime),
               dayofweek = wday(datetime, label = TRUE, abbr = TRUE),
               hour = hour(datetime)
        ) %>% 
        select(-cmplnt_fr_dt, -cmplnt_fr_tm)
    
    return(df_cleaned)
}

df_cleaned <- clean_data(df)

offenses <- unique(df_cleaned$law_cat_cd)

boroughs <- unique(df_cleaned$boro_nm)

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
                start = "2019-01-01",
                end = "2020-12-31",
                min = "2019-01-01",
                max = "2020-12-31"
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
                choices = c("All", offenses),
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
            ),
            
            actionButton(
                inputId = "go",
                label = "Retrieve Data"
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
                choices = boroughs,
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