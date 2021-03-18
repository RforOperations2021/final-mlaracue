library("shiny")
library("shinythemes")
library("plotly")
library("leaflet")
library("leaflet.extras")

# library("readr")
library("tidyr")
library("dplyr")
library("lubridate")

# -- data loading
# shapefile <- readOGR('https://data.cityofnewyork.us/api/geospatial/cpf4-rkhq?method=export&format=GeoJSON')

load("dummy-data.Rdata")

ntas_data <- shapefile@data

my_pal <- c('#4cc9f0', "#3a0ca3", "#f72585")

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
    
    # themeSelector(),
    theme = shinytheme("darkly"),
    
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
                label = "Search by:", 
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
                    
                    plotlyOutput(outputId = "timeseries", height = "40%"),
                    
                    fluidRow(
                        
                        column(
                            width = 8,
                            
                            radioButtons(
                                inputId = "type", 
                                label = "Type of map:", 
                                choices = c("Heatmap", "CircleMarkers"), 
                                selected = "Heatmap", 
                                inline = TRUE
                            ),
                            
                            leafletOutput(outputId = "map", width = "80%", height = "600px")
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
    
    # --the ui options change depending on whether user wants to filter data based on boroughs
    # or a point location and a radius
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
    
    output$timeseries <- renderPlotly(
        
        df_cleaned %>%
            count(datetime, name = "n_crimes") %>% 
            plot_ly(
                x = ~datetime, 
                y = ~n_crimes, 
                height = 150,
                type = 'scatter',
                mode = "lines+markers",
                marker = list(color = my_pal[1]),
                line = list(color = my_pal[1])
            ) %>% 
            layout(
                xaxis = list(title = 'Date'),
                yaxis = list(title = 'No. of Crimes'),
                margin = list(l = 10, r = 10, t = 10, b = 10),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                font = list(color = '#FFFFFF', size = 10)
            )
    )
    
    params <- reactiveValues(
        lat = mean(df_cleaned$latitude),
        lng = mean(df_cleaned$longitude)
    )
    
    df_merged <- reactive({
        df_cleaned %>%
            st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
            group_by(id) %>%
            mutate(geometry = st_combine(geometry)) %>%
            ungroup() %>%
            st_join(st_as_sf(shapefile))
    })
    
    # -- basic map
    output$map <- renderLeaflet({
        
        leaflet(data = shapefile) %>% 
            addProviderTiles(provider = "Stamen.Toner") %>%
            setView(
                zoom = 12, 
                lat = params$lat, 
                lng = params$lng
            )
        
    })
    
    crime_count <- reactive({
        
        crimes_count <- df_merged() %>%
            st_set_geometry(NULL) %>%
            count(ntacode, sort = TRUE, name = "n_crimes")
        
        shapefile@data <- merge(ntas_data, crimes_count, sort = FALSE, by = "ntacode")
        
        return(shapefile)
        
    })
    
    # heatmap <- reactive({
    #     
    #     ntas <- crime_count()
    #     
    #     bins <- seq(from = 0, to = max(ntas@data$n_crimes), by = 1)
    #     
    #     pal <- colorBin("BuPu", domain = ntas@data$n_crimes, bins = bins)
    #     
    #     leafletProxy("map", data = ntas) %>% 
    #         addPolygons(
    #             fillColor = ~ pal(n_crimes),
    #             popup = ~ paste0("<b>", ntaname, ":</b> ", n_crimes, " crime(s)"),
    #             weight = 2,
    #             opacity = 1,
    #             color = "black",
    #             dashArray = "1",
    #             fillOpacity = 0.85
    #         ) %>%
    #         addLegend(
    #             pal = pal,
    #             values = ~n_crimes,
    #             opacity = 0.85,
    #             position = "bottomright",
    #             title = "Number of crimes by NTA"
    #         )
    # })
    # 
    # circles <- reactive({
    #     
    #     heatmap_data <- df_cleaned %>%
    #         select(law_cat_cd, ofns_desc, longitude, latitude)
    #     
    #     pal2 <- colorFactor(my_pal, c("Misdemeanor", "Felony", "Violation"))
    #     
    #     leafletProxy("map", data = heatmap_data) %>%
    #         addCircleMarkers(
    #             lng = ~longitude,
    #             lat = ~latitude,
    #             radius = 10,
    #             popup = ~ ofns_desc,
    #             color = ~pal2(law_cat_cd)
    #         ) %>%
    #         addLegend(
    #             pal = pal2,
    #             values = heatmap_data$law_cat_cd,
    #             title = "Level of Offense"
    #         )
    # })
    
    # -- replace layer according to user inputs
    observe({
        
        if(input$type == "Heatmap"){
            
            leafletProxy("map") %>%
                clearMarkers() %>%
                clearControls()
            
            ntas <- crime_count()
            
            bins <- seq(from = 0, to = max(ntas@data$n_crimes), by = 1)
            
            pal <- colorBin("BuPu", domain = ntas@data$n_crimes, bins = bins)
            
            leafletProxy("map", data = ntas) %>% 
                addPolygons(
                    fillColor = ~ pal(n_crimes),
                    popup = ~ paste0("<b>", ntaname, ":</b> ", n_crimes, " crime(s)"),
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    dashArray = "1",
                    fillOpacity = 0.85
                ) %>%
                addLegend(
                    pal = pal,
                    values = ~n_crimes,
                    opacity = 0.85,
                    position = "bottomright",
                    title = "Number of crimes by NTA"
                )
            
        } else if(input$type == "CircleMarkers"){

            leafletProxy("map") %>%
                clearShapes() %>%
                clearControls()
            
            heatmap_data <- df_cleaned %>%
                select(law_cat_cd, ofns_desc, longitude, latitude)
            
            pal2 <- colorFactor(my_pal, c("Misdemeanor", "Felony", "Violation"))
            
            leafletProxy("map", data = heatmap_data) %>%
                addCircleMarkers(
                    lng = ~longitude,
                    lat = ~latitude,
                    radius = 10,
                    popup = ~ ofns_desc,
                    color = ~pal2(law_cat_cd)
                ) %>%
                addLegend(
                    pal = pal2,
                    values = heatmap_data$law_cat_cd,
                    title = "Level of Offense"
                )
        }
    })
    
}

# run the application
shinyApp(ui = ui, server = server)