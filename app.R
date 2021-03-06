library("shiny")
library("shinythemes")
library("DT")
library("plotly")
library("leaflet")
library("leaflet.extras")
library("R.utils")

library("sf")
library("tidyr")
library("dplyr")
library("stringr")
library("lubridate")

library("yaml")
library("RSocrata")

# -- data loading
access_info <- yaml.load_file(input = "credentials.yaml")

info <- readLines(con = "about.txt")

# shapefile <- readOGR('https://data.cityofnewyork.us/api/geospatial/cpf4-rkhq?method=export&format=GeoJSON')
# shapefile is loaded as an .RData object to enhance app response time
load("shapefile.Rdata")

# my_pal <- c('#4cc9f0', "#3a0ca3", "#f72585")
my_pal <- rev(c('#007f5f', "#aacc00", "#ffff3f"))

clean_data <- function(df) {
    df_cleaned <- df %>% 
        mutate_at(.vars = c("boro_nm", "law_cat_cd", "ofns_desc"),
                  .funs = str_to_title) %>% 
        mutate_at(.vars = c("latitude", "longitude"),
                  .funs = as.numeric) %>% 
        mutate(id = 1:nrow(.),
               cmplnt_fr_dt = str_replace_all(
                   cmplnt_fr_dt, 
                   pattern = "T00:00:00.000",
                   replacement = ""),
               datetime = paste(cmplnt_fr_dt, " ", cmplnt_fr_tm),
               datetime = as_datetime(datetime),
               dayofweek = wday(datetime, label = TRUE, abbr = TRUE),
               hour = hour(datetime)
        ) %>% 
        select(-cmplnt_fr_dt, -cmplnt_fr_tm) %>% 
        filter(!is.na(boro_nm))
    
    return(df_cleaned)
}

# -- function that creates a query based on user inputs
myquery <- function(dates, borough, offense, size = NULL) ({
    
    query <- paste0(
        "https://data.cityofnewyork.us/resource/5uac-w243.json?$", # url
        "select=boro_nm,cmplnt_fr_dt,cmplnt_fr_tm,law_cat_cd,ofns_desc,latitude,longitude&$", # columns
        sprintf(
            fmt = "where=cmplnt_fr_dt between '%sT00:00:00.000' and '%sT00:00:00.000' AND boro_nm='%s'", 
            dates[1], dates[2], str_to_upper(borough)
        )
    )
    
    if(offense != "All"){
        
        query <- paste0(
            query,
            sprintf(fmt = " AND law_cat_cd='%s'", str_to_upper(offense))
        )
    } 
    
    if(!is.null(size)){
        query <- paste0(
            query,
            sprintf(fmt = "&$limit=%s", size)
        )
    }
    
    return(query)
})



max_size <- 100

# df_cleaned <- clean_data(df) %>% filter(datetime >= "2020-12-24")

ui <- fluidPage(
    
    titlePanel(title = "NYPD Explorer"),
    
    h4("Analysis of last six months of crime data in NYC"),
    
    # themeSelector(),
    theme = shinytheme("darkly"),
    
    tags$head(tags$style("div.dataTables_scrollHead span {color: black;}")),
    
    sidebarLayout(
        
        sidebarPanel(
            
            width = 3,
            
            dateRangeInput(
                inputId = "dates", 
                label = "Date range:",
                start = "2020-12-31",
                end = "2020-12-31",
                min = "2020-07-01",
                max = "2020-12-31"
            ),
            
            selectInput(
                inputId = "borough",
                label = "Borough:",
                choices = c("Manhattan", "Brooklyn", "Bronx", "Queens", "Staten Island"),
                multiple = FALSE
            ),
            
            selectInput(
                inputId = "offense",
                label = "Level of offense:",
                choices = c("All", "Misdemeanor", "Felony", "Violation"),
                selected = "All",
                multiple = FALSE
            ),
            
            uiOutput(outputId = "doSampling"),
            
            downloadButton(
                outputId = 'download', 
                label = "Download data"
            )
        ),
        
        mainPanel(
            width = 9,
            
            tabsetPanel(
                selected = "Plots",
                
                tabPanel(
                    
                    title = "Plots",
                    
                    plotlyOutput(outputId = "timeseries", height = "40%"),
                    
                    textOutput(outputId = "n_obs", inline = TRUE),
                    
                    hr(),
                    
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
                
                tabPanel(title = "Data", DTOutput("table")),
                
                tabPanel(title = "About", HTML(text = info))
            )
        )
    )
)

server <- function(input, output, session) {
    
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
    
    # --the maximum range available (to avoid large requests) is one month
    observe({
        req(input$dates)
        
        start <- as_date(input$dates[1])
        end <- as_date(input$dates[2])
        
        if(month(end) != month(start) & (end - start) > days_in_month(month(start))){
            
            new_end <- start + days(30)
            
            updateDateRangeInput(
                session = session,
                inputId = "dates",
                end = new_end
            )
            
            showNotification(
                ui = "The maximum number of days you can filter is 30", 
                type = "warning"
            )
        }
    })
    
    # -- make the request to Socrata. Return null if time execution is greater than 1 second
    df <- reactive({
        
        withTimeout(
            expr = read.socrata(
                url = myquery(input$dates, input$borough, input$offense),
                app_token = access_info$app_token,
                email = access_info$email,
                password = access_info$password
            ), 
            timeout = 1, 
            onTimeout = "silent"
        )
        
    })
    
    # -- if not possible to retrieve all data (returns an error), activate sampling
    output$doSampling <- renderUI({
        
        if (is.null(df())) {
            
            sliderInput(
                inputId = "size",
                label = "Sample size:",
                value = 1,
                min = 1,
                max = max_size,
                step = 5,
                post = "K"
            )
        }
        
    })
    
    # -- let know user that sample size option was activated
    observe({
        
        if (is.null(df())) {
            
            showModal(modalDialog(
                title = "Attention!", 
                "The data you're trying to retrieve is too large. The first 100 will be selected.
                You can select other sample size."
            ))
        }
    })
    
    # --show a modal when the sample size is greater than 500
    observe({
        
        if(!is.null(input$size)) {
            
            if(input$size * 100 > 1000){
                
                showModal(modalDialog(
                    title = "Warning!", 
                    "Sample size might be too large.
                    It would take some time to retrieve the data. Be patient!"
                ))
            }
            
        }
    })
    
    # -- avoid sample size to be greater than max sample size
    # observe({
    #     
    #     req(input$size)
    #     
    #     if(!is.null(input$size)){
    #         
    #         if(input$size > max_size){
    #             
    #             updateSliderInput(
    #                 session = session,
    #                 inputId = "size",
    #                 value = max_size
    #             )
    #             
    #             showNotification(
    #                 ui = "The limit of sample size is 10,000", 
    #                 type = "warning"
    #             )
    #         }
    #     }
    # })
    
    # if the time execution is greater than 1 second (df is null), tries with a sample size
    df_cleaned <- reactive({
        
        if(!is.null(df())){
            
            clean_data(df())
            
        } else {
            
            req(input$size)
            
            clean_data(
                read.socrata(
                    url = myquery(input$dates, input$borough, input$offense, (input$size * 100)),
                    app_token = access_info$app_token,
                    email = access_info$email,
                    password = access_info$password
                )
            )
        }
    })
    
    # -- display number of obs
    output$n_obs <- renderText({
        
        paste0("Total Number of Observations: ", scales::comma(nrow(df_cleaned())))
    })
    
    # -- let know user if API retrieves empty data
    observe({
        
        req(df_cleaned())
        
        if(nrow(df_cleaned()) == 0){
            
            showModal(modalDialog(
                title = "Error!", 
                "There is no data with the specified filters. Please try again."
            ))
        }
    })
    
    # -- time series for plot on the top of the app
    output$timeseries <- renderPlotly(
        
        df_cleaned() %>%
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
    
    # -- crime data merged with NTAs Polygons dataframe
    df_merged <- reactive({
        
        df_cleaned() %>%
            st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
            group_by(id) %>%
            mutate(geometry = st_combine(geometry)) %>%
            ungroup() %>%
            st_join(st_as_sf(shapefile))
    })
    
    # -- basic map
    output$map <- renderLeaflet({
        
        leaflet() %>% 
            addProviderTiles(provider = "Stamen.Toner") %>%
            setView(
                zoom = 11, 
                # change lat and lon based on Borough
                lat = mean(df_cleaned()$latitude), 
                lng = mean(df_cleaned()$longitude)
            )
        
    })
    
    # -- compute crime counts by ntacode and append to shapefile
    crime_count <- reactive({
        
        crimes_count <- df_merged() %>%
            st_set_geometry(NULL) %>%
            count(ntacode, sort = TRUE, name = "n_crimes")
        
        ntas_data <- shapefile[shapefile$ntacode %in% crimes_count$ntacode, ]
        
        ntas_data@data <- merge(shapefile@data, crimes_count, sort = FALSE, by = "ntacode")
        
        return(ntas_data)
        
    })
    
    # -- replace layer according to user inputs
    observe({
        
        if(input$type == "Heatmap"){
            
            # remove previous layers so they're not displayed on top of each other
            leafletProxy("map") %>%
                clearMarkers() %>%
                clearControls()
            
            ntas <- crime_count()
            
            # -- create the number of ranges programmatically. 
            # This ensures that the number of levels is not too large (when number of crimes is high)
            # not too short either (when number of crimes is less than 20 or so)
            
            n_max <- max(ntas@data$n_crimes)
            
            n_max <- if(n_max > 100) round(n_max, digits = -2) + 50 else round(n_max, digits = -1) + 5
            
            bins <- seq(from = 0, to = n_max, by = n_max / 5)
            
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
            heatmap_data <- df_cleaned() %>%
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
    
    # -- barplot with the total number of crimes by day of the week
    output$barplot <- renderPlotly(
        
        if(input$offense2 == "All"){
            
            fig <- df_cleaned() %>%
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
            
            fig <- df_cleaned() %>%
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
    
    # -- lineplot with the total number of crimes by hour of the day
    output$lineplot <- renderPlotly(
        
        if(input$offense2 == "All"){
            
            fig <- df_cleaned() %>%
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
            
            fig <- df_cleaned() %>%
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
    
    # -- data table in second tab
    output$table <- renderDT({
        
        data <- df_cleaned() %>% select(-id)
        
        datatable(
            data = data,
            caption = "NYPD queried data",
            options = list(
                initComplete = JS(
                    # to change the color of the text in column names
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#282828', 'color': '#fff'});",
                    "}"),
                pageLength = 50
            ),
            rownames = FALSE
        ) %>% 
            formatRound(columns = c(4:5), digits = 2) %>% 
            # to change background color and font color of text rows
            formatStyle(columns = colnames(data),
                        backgroundColor = '#282828', 
                        color = "white"
            )
    })
    
    # download data. In file name the date range is indicated
    output$download <- downloadHandler(
        
        filename = function() {
            paste0('NYPD-crime-data', input$dates[1], 'to', input$dates[2], '.csv')
        },
        content = function(file){
            write.csv(df_cleaned() %>% select(-id), file, row.names = FALSE)
        }
    )
    
}

# run the application
shinyApp(ui = ui, server = server)