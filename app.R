library("shiny")
library("shinythemes")
library("plotly")
library("leaflet")
library("leaflet.extras")

library("sf")
library("tidyr")
library("dplyr")
library("stringr")
library("lubridate")

# -- data loading
# shapefile <- readOGR('https://data.cityofnewyork.us/api/geospatial/cpf4-rkhq?method=export&format=GeoJSON')

load("dummy-data.Rdata")

ntas_data <- shapefile@data # to preserve original data and use it for merging

# my_pal <- c('#4cc9f0', "#3a0ca3", "#f72585")
my_pal <- rev(c('#007f5f', "#aacc00", "#ffff3f"))

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
        select(-cmplnt_fr_dt, -cmplnt_fr_tm) %>% 
        filter(datetime >= "2020-12-24")
    
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
                            
                            leafletOutput(outputId = "map", height = "600px")
                        ),
                        
                        column(
                            width = 4,
                            
                            uiOutput("showopts"),
                            
                            br(),
                            
                            h5("Number of Crimes by Day of the Week"),
                            
                            plotlyOutput(outputId = "barplot", height = "250px"),
                            
                            hr(),
                            
                            h5("Number of Crimes by Hour of the Day"),
                            
                            plotlyOutput(outputId = "lineplot", height = "250px")
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
    
    # -- this filter is only shown when no specific level of offense is selected
    output$showopts <- renderUI({
        
        if(input$offense == "All"){
            
            radioButtons(
                inputId = "offense2",
                label = "Select one:",
                choices = c('All', 'Level of offense'),
                selected = 'All',
                inline = TRUE
            )
            
        }
    })
    
    # -- time series for plot on the top of the app
    output$timeseries <- renderPlotly(
        
        df_cleaned %>%
            count(datetime, name = "n_crimes") %>% 
            plot_ly(
                x = ~datetime, 
                y = ~n_crimes, 
                height = 150,
                type = 'scatter',
                mode = "lines",
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
    
    # -- reactive values with mean of lat and lng based on data obs
    params <- reactiveValues(
        lat = mean(df_cleaned$latitude),
        lng = mean(df_cleaned$longitude)
    )
    
    # -- crime data merged with NTAs Polygons dataframe
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
                zoom = 11, 
                lat = params$lat, 
                lng = params$lng
            )
        
    })
    
    # -- compute crime counts by ntacode and append to shapefile
    crime_count <- reactive({
        
        crimes_count <- df_merged() %>%
            st_set_geometry(NULL) %>%
            count(ntacode, sort = TRUE, name = "n_crimes")
        
        shapefile@data <- merge(ntas_data, crimes_count, sort = FALSE, by = "ntacode")
        
        return(shapefile)
        
    })
    
    # -- replace layer according to user inputs
    observe({
        
        if(input$type == "Heatmap"){
            
            # remove previous layers so they're not displayed on top of each other
            leafletProxy("map") %>%
                clearMarkers() %>%
                clearControls()
            
            ntas <- crime_count()
            
            crimes_std <- var(ntas@data$n_crimes) %>% sqrt() %>% round()
            
            bins <- seq(from = 0, to = max(ntas@data$n_crimes), by = crimes_std)
            
            pal <- colorBin("viridis", domain = ntas@data$n_crimes, bins = bins)
            
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
            
            # remove previous layers so they're not displayed on top of each other
            leafletProxy("map") %>%
                clearShapes() %>%
                clearControls()
            
            # for the markers, I use lat, lng coordinates from filtered df.
            heatmap_data <- df_cleaned %>%
                select(law_cat_cd, ofns_desc, longitude, latitude)
            
            pal2 <- colorFactor(my_pal, c("Misdemeanor", "Felony", "Violation"))
            
            leafletProxy("map", data = heatmap_data) %>%
                addCircleMarkers(
                    lng = ~longitude,
                    lat = ~latitude,
                    radius = 5,
                    popup = ~ ofns_desc,
                    color = ~pal2(law_cat_cd),
                    opacity = 0.7
                ) %>%
                addLegend(
                    pal = pal2,
                    values = heatmap_data$law_cat_cd,
                    title = "Level of Offense"
                )
        }
    })
    
    output$barplot <- renderPlotly(
        
        if(input$offense2 == "All"){
            
            fig <- df_cleaned %>%
                count(dayofweek, name = "n_crimes") %>% 
                plot_ly(
                    y = ~dayofweek, 
                    x = ~n_crimes, 
                    height = 200,
                    type = 'bar',
                    marker = list(color = my_pal[1])
                ) %>% 
                layout(
                    yaxis = list(title = 'Day of the Week'),
                    xaxis = list(title = 'No. of Crimes'),
                    margin = list(l = 10, r = 10, t = 10, b = 10),
                    plot_bgcolor  = "rgba(0, 0, 0, 0)",
                    paper_bgcolor = "rgba(0, 0, 0, 0)",
                    font = list(color = '#FFFFFF', size = 10)
                )
            
        } else {
            
            fig <- df_cleaned %>%
                count(law_cat_cd, dayofweek, name = "n_crimes") %>% 
                plot_ly(
                    y = ~dayofweek, 
                    x = ~n_crimes, 
                    color = ~law_cat_cd,
                    height = 200,
                    type = 'bar',
                    colors = my_pal
                ) %>% 
                layout(
                    yaxis = list(title = 'Day of the Week'),
                    xaxis = list(title = 'No. of Crimes'),
                    margin = list(l = 10, r = 10, t = 10, b = 5),
                    plot_bgcolor  = "rgba(0, 0, 0, 0)",
                    paper_bgcolor = "rgba(0, 0, 0, 0)",
                    font = list(color = '#FFFFFF', size = 10),
                    legend = list(orientation = 'h', y = -0.5),
                    barmode = 'stack'
                )
        }

    )
    
    output$lineplot <- renderPlotly(
        
        if(input$offense2 == "All"){
            
            fig <- df_cleaned %>%
                count(hour, name = "n_crimes") %>% 
                plot_ly(
                    x = ~hour, 
                    y = ~n_crimes, 
                    height = 200,
                    type = 'scatter',
                    mode = "lines",
                    marker = list(color = my_pal[1]),
                    line = list(color = my_pal[1])
                ) %>% 
                layout(
                    yaxis = list(title = 'No. of Crimes'),
                    xaxis = list(title = 'Hour'),
                    margin = list(l = 10, r = 10, t = 10, b = 10),
                    plot_bgcolor  = "rgba(0, 0, 0, 0)",
                    paper_bgcolor = "rgba(0, 0, 0, 0)",
                    font = list(color = '#FFFFFF', size = 10)
                )
            
        } else {
            
            fig <- df_cleaned %>%
                count(law_cat_cd, hour, name = "n_crimes") %>% 
                plot_ly(
                    x = ~hour, 
                    y = ~n_crimes, 
                    color = ~law_cat_cd,
                    height = 200,
                    type = 'scatter',
                    mode = "lines",
                    colors = my_pal,
                    line = list(color = my_pal)
                ) %>% 
                layout(
                    yaxis = list(title = 'No. of Crimes'),
                    xaxis = list(title = 'Hour'),
                    margin = list(l = 10, r = 10, t = 10, b = 5),
                    plot_bgcolor  = "rgba(0, 0, 0, 0)",
                    paper_bgcolor = "rgba(0, 0, 0, 0)",
                    font = list(color = '#FFFFFF', size = 10),
                    legend = list(orientation = 'h', y = -0.5),
                    barmode = 'stack'
                )
        }
        
    )
    
}

# run the application
shinyApp(ui = ui, server = server)