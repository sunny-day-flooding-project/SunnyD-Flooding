# Packages to load
library(tidyverse)
library(shiny)
library(shinyjs)
library(rsconnect)
library(shinythemes)
library(shinydashboard)
library(colourvalues)
library(waiter)
library(sf)
library(leaflet)
library(raster)
library(rgdal)
library(lwgeom)
library(ggrepel)
library(DT)
library(htmltools)
library(RColorBrewer)
library(lubridate)
library(plotly)

# City names to show on initial load map
place_names <- c("Beaufort, North Carolina", "Carolina Beach, North Carolina")

# Boundaries of locations listed in "place_names"
urban_boundaries <- sf::st_read("data/merged_boundaries_coast.shp") %>%
    sf::st_make_valid() %>%
    sf::st_cast("POLYGON") %>%
    mutate(place = paste0(Place_Name,", ",State_Name)) %>% 
    filter(place %in% place_names) %>% 
    group_by(place) %>% 
    summarise() %>% 
    sf::st_as_sf() %>% 
    st_centroid() 

# HTML waiting screen for initial load
waiting_screen <- tagList(
    spin_ring(),
    h4("Cool stuff loading...")
)

# Color palette for water level on site map
pal <- colorNumeric(
    palette = rev(brewer.pal(10,"RdBu")),
    domain = c(-3,3))

# Sensor lat and long data
sensor_locations <- tibble::tibble("place" = c("Beaufort, North Carolina", "Beaufort, North Carolina", "Carolina Beach, North Carolina", "Carolina Beach, North Carolina"),
                                   "sensor_ID" = c("BF_1","BF_2","CB_1", "CB_2"),
                                   "lat" = c(34.715890,34.714741,34.052327, 34.053141), 
                                   "long" = c(-76.663831, -76.661725, -77.885139, -77.884629)) %>% 
    sf::st_as_sf(coords = c("long","lat"), crs = 4269) 

# Loads fake data
database <- sensor_locations %>% tibble::as_tibble() %>% slice(rep(1:n(), each=1000)) %>% 
    mutate(level = sin(row_number()) + rnorm(n=10,mean=0, sd=.1)) %>% 
    mutate(date = rep(seq(from = ymd_hms(Sys.time(),tz="EST") - days(40),by ="hours", length.out = 1000),nrow(sensor_locations)))

# Update sensor locations with most recent data from database, also construct html popups
sensor_locations <- sensor_locations %>% 
    left_join(database %>% 
                  group_by(sensor_ID) %>% 
                  arrange(desc(date)) %>%
                  slice(1)) %>% 
    mutate(html_popups = paste0(
        '<div align=\"center\">',
        '<h3>Site ',sensor_ID,'</h3>',
        '<h4>Current level: ', round(level, digits = 2),'</h4>',
        '<button id="view_site" type="button" class="btn btn-default action-button" onclick="{Shiny.onInputChange(&#39;view_site&#39;, (Math.random() * 1000) + 1);}">View data</button></div>'))

#------------------------ Define UI ---------------------------------------
ui <- tagList(
    tags$style(".fa-3x {color:#18bc9c"),
    navbarPage(
    title = "SunnyD flooding project", 
    id = "nav",
    theme = shinythemes::shinytheme("flatly"),
    
    # Home page tab has information about project
    tabPanel(
        title = "Home",
        value = "Home",
        div(
            align = "center",
            style = "background-image: linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url('beaufort_flooding.jpg');
                 background-position: center;
                 background-repeat: no-repeat;
                 background-size: cover;
                 margin-top: -22px;
                 margin-left: -15px;
                 margin-right: -15px;
                 height: 40vh;

             ",
            div(
                style = "position: absolute;
             top: 25%;
             left: 50%;
                 transform: translate(-50%, -50%);",
                h1("SunnyD Flooding Project", style = "color:white"),
                p(
                    "Measuring recurrent coastal flooding with real-time sensor networks",
                    style = "color:white"
                ),
                br(),
                br(),
                actionButton("click", label = "View our sites", class = "btn-success"),
            )
        ),
        br(),
        br(),
        fluidRow(
            column(
                width = 4,
                icon("cloud", "fa-3x"),
                h2("Sensors"),
                p(
                    "Constructing a real-time sensor network in storm drains to measuring recurrent coastal flooding caused by tides, winds, and storm surge"
                ),
                align = "center"
            ),
            column(
                width = 4,
                icon("laptop", "fa-3x"),
                h2("Modeling"),
                p(
                    "Using computer models to understand drivers of recurrent flooding so we can better predict future conditions"
                ),
                align = "center"
            ),
            column(
                width = 4,
                icon("home", "fa-3x"),
                h2("Community"),
                p(
                    "Engaging with stakeholders and community members to design our projects and inform public policy"
                ),
                align = "center"
            )
        )
    ), 

    # Mapping tab for overview and site map
    tabPanel(
        title = "Map",
        value = "Map",
        waiter::use_waiter(),
        waiter::waiter_show_on_load(html = waiting_screen),
        sidebarLayout(sidebarPanel(
            div(
                align = "center",
                h2("SunnyD Flooding Project"),
                helpText("Measuring sunny day flooding in increase coastal resilience"),
                br(),

                # Select city, either Beaufort or Carolina Beach
                selectizeInput(
                    'city_name',
                    'Location',
                    choices = place_names,
                    options = list(
                        placeholder = 'Please select an option below',
                        onInitialize = I('function() { this.setValue(""); }')
                    )
                ),
                br(),
                br(),
                br(),
                br(),
                plotOutput("scatterplot", height = "40vh")
            )
        ),
        
        # Panel with map
        mainPanel(div(
            tags$style(type = "text/css", "#m {height: calc(100vh - 90px) !important;}"),
            leafletOutput(outputId = "m", height = "100vh"),
        )))
    ), 
    
    # Tab showing selected data and time series graphs
    tabPanel(title = "Data",
             value = "Data",
             sidebarLayout(
                 sidebarPanel(
                     
                     # Selecting City and then sensor station
                     uiOutput("firstSelection", align = "center"),
                     uiOutput("secondSelection", align = "center"),
                     
                     # filter by date, auto is the last three days of data
                     dateRangeInput(
                         'dateRange',
                         label = 'Date range input: yyyy-mm-dd',
                         start = Sys.Date() - 3,
                         end = Sys.Date()
                     ),
                     
                     # Small map showing all of the sites
                     leafletOutput(outputId = "m2", height = "40vh")
                 ),
                 
                 # Time series interactive plot or data table in separate tabs
                 mainPanel(tabsetPanel(
                     id = "data_tabs",
                     
                     tabPanel(
                         "Plot",
                         plotlyOutput("site_level_ts", height = "40vh"),
                         uiOutput("site_data_panel")
                     ),
                     tabPanel("Table",
                              DT::dataTableOutput("site_data"))
                 ))
             ))#, 
    
    # Adds copyright to
    # tags$script(HTML("var header = $('.navbar> .container-fluid');
    #                    header.append('<div style=/'float:right;color:white/'><h5>&#169 Adam C. Gold, 2020</h5></div> !important');
    #                    console.log(header)"))
))



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Initialize wait screen for plotting data
    w <- Waiter$new(id="site_level_ts")
    
    # If button on home page (View our sites) is clicked, will switch navbar tab to "Map"
    observeEvent(input$click, {
        updateNavbarPage(session,
                         inputId = "nav",
                         selected = "Map")
    })  
    
    # Input drop-downs menus for data page
    output$firstSelection <- renderUI({
        selectInput("data_location", "Select a location", choices = unique(sensor_locations$place), selected = reactive_selection$overall_data_location, selectize = T)
    })

    output$secondSelection <- renderUI({
        selectInput("data_sensor", "Select a sensor", choices = reactive_selection$overall_data_sensor_choices, selected = reactive_selection$overall_data_sensor,selectize = T,multiple = T)
    })
    
    # Set the reactive values that will be updated by user inputs
    reactive_selection <- reactiveValues(overall_data_sensor = "BF_1",
                                         overall_data_location = "Beaufort, North Carolina",
                                         overall_data_sensor_choices = c("BF_1","BF_2"),
                                         data_sensor = "BF_1",
                                         data_location = "Beaufort, North Carolina")
    
    # Reactive value that stores the data from the selected sensor site, filtered by date range
    sensor_data <- reactive({
        req(input$data_sensor)
        database %>% 
            filter(sensor_ID %in% input$data_sensor) %>%
            filter(date >= min(input$dateRange, na.rm=T) & date <= max(input$dateRange, na.rm=T)) %>% 
            dplyr::select(-geometry)
    })
    
    
    # Create initial map for "map" tab 
    output$m <-renderLeaflet(leaflet() %>%
                                 addProviderTiles(group = "Positron (default)",provider = providers$CartoDB.Positron) %>%
                                 addProviderTiles(group = "Dark Matter",provider = providers$CartoDB.DarkMatter) %>%
                                 addProviderTiles(group = "Imagery",provider = providers$Esri.WorldImagery) %>%
                                 addTiles(group = "OSM") %>%
                                 setView(lng = -77.360784, lat = 34.576053, zoom = 8) %>%
                                 addCircleMarkers(data = sensor_locations, group = "sensor_site",
                                                  popup = ~html_popups,
                                                  clusterOptions = markerClusterOptions(),
                                                  clusterId = "place",
                                                  layerId = sensor_locations$sensor_ID,
                                                  color = "black", fillColor = ~pal(level), fillOpacity = 1) %>% 
                                 addLegend('bottomright', pal = pal, values = c(-3,3),
                                           title = 'Current water<br>level relative<br>to surface',
                                           opacity = 1) %>% 
                                 addLayersControl(
                                     baseGroups = c("Positron (default)", "Dark Matter","Imagery", "OSM"),
                                     # overlayGroups = c("overview_locations"),
                                     options = layersControlOptions(collapsed = FALSE)) %>% 
                                 addEasyButton(easyButton(
                                     icon="fa-globe", title="Zoom to Level 4",
                                     onClick=JS("function(btn, map){ map.setZoom(4); }"),
                                     position = "topright")) 
    )
    
    # reactive value storing the mean coordinates of the sensors from the city selected from the map tab
    map1_selected_location <- reactive({
        req(input$city_name)
        reactive_selection$overall_data_location <- input$city_name
        
        sensor_locations %>% 
            filter(place == input$city_name) %>% 
            sf::st_coordinates() %>% 
            as_tibble() %>% 
            summarise(lng = mean(X, na.rm=T),
                      lat = mean(Y, na.rm=T))
    })
    
    # If city name is selected on the map tab, fly to the location
    observeEvent(input$city_name,{
        leafletProxy(mapId = "m") %>% 
            flyTo(lng = map1_selected_location()[1], lat = map1_selected_location()[2], zoom=16)
    })
    
    # Sensor selected from map tab. Uses map click to update reactive value that controls dropdown menus on Data tab
    map1_clicked_sensor <- reactive({
        req(input$m_marker_click)
        # reactive_selection$overall_data_sensor <- input$m_marker_click #Possibly not needed, but keeping for now as comment
        input$m_marker_click
        })
    
    # Updates the choices for the sensor ID on the data tab
    observe({
        reactive_selection$overall_data_sensor_choices <- sensor_locations$sensor_ID[sensor_locations$place == input$data_location]
    })
    
    # Updates the reactive values for data sensor and location on the Data tab using click on Map tab
    observe({
        reactive_selection$overall_data_sensor <- input$m_marker_click
        reactive_selection$overall_data_location <- unique(sensor_locations$place[sensor_locations$sensor_ID == input$m_marker_click$id])
    })

    # If location is selected on dropdown menus on Data tab, calculate the mean coordinates for the location
    map2_selected_location <- reactive({
        req(input$data_location)

        sensor_locations %>%
            filter(place == input$data_location) %>%
            sf::st_coordinates() %>%
            as_tibble() %>%
            summarise(lng = mean(X, na.rm=T),
                      lat = mean(Y, na.rm=T))
    })
    
    # Select sensors using dropdown menu
    map2_selected_sensors <- reactive({
        req(input$data_sensor)
        
        sensor_locations %>%
            filter(sensor_ID %in% input$data_sensor)
    })
    
    # Create map for Data tab
    output$m2 <-renderLeaflet(leaflet() %>%
                                 addProviderTiles(group = "Positron (default)",provider = providers$CartoDB.Positron) %>%
                                  addCircleMarkers(data = sensor_locations ,
                                            group = "overview_locations",
                                            layerId = ~sensor_ID,
                                            label = ~sensor_ID,
                                            color = "black",
                                            labelOptions = labelOptions(noHide = F))
                              )
    # If location is selected on Data tab dropdown, change the map view
    observeEvent(input$data_location,{
        leafletProxy(mapId = "m2") %>%
            setView(lng = map2_selected_location()[1], lat = map2_selected_location()[2], zoom=15)
    })
    
    # Create a red point for selected sensor
    observe({
        # print(map2_selected_sensors())
        leafletProxy(mapId = "m2") %>%
        clearGroup(group = "selected_site_map2") %>%
        addCircleMarkers(data = map2_selected_sensors(),
                         group = "selected_site_map2",
                   label = ~sensor_ID,
                   color = "red")
        })
    
    # Store ID of clicked point on map on Data tab
    map2_clicked_sensor <- reactive({
        req(input$m2_marker_click)
        reactive_selection$overall_data_sensor <- input$m2_marker_click
            
        input$m2_marker_click
        })
    
    # Update reactive value for dropdown menu using map 2 marker click
    observe({
        reactive_selection$overall_data_sensor <- input$m2_marker_click
    })
    
    # View site by switching tab
        observeEvent(input$view_site, {
            updateNavbarPage(session, 
                         inputId = "nav",
                         selected = "Data")
        })
          
        # Render plot with selected data
        observe({
            output$site_level_ts <- renderPlotly(
                ggplotly(ggplot(data = sensor_data())+
                geom_line(aes(x=date,y=level, color = sensor_ID, group = sensor_ID))+
                    geom_point(aes(x=date,y=level,color=sensor_ID))+
                    theme_minimal()+
                    xlab("Date")+
                    ylab("Water level (ft)")+
                    theme(text=element_text(family = "Bahnschrift", size = 16),
                          legend.title = element_blank())+
                    scale_x_datetime())
            )
            
        # Render table with selected data
            output$site_data <- renderDataTable(sensor_data())
        })


        waiter_hide()
        
}

# Run the application
shinyApp(ui = ui, server = server)
