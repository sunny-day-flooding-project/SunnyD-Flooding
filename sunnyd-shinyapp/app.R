# Packages to load
library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(colourvalues)
library(waiter)
library(sf)
library(leaflet)
library(raster)
library(rgdal)
library(lwgeom)
library(DT)
library(htmltools)
library(RColorBrewer)
library(plotly)
library(RPostgres)
library(DBI)
library(pool)
library(magick)
library(shinyalert)


# City names to show on initial load map
place_names <- c("Beaufort, North Carolina", "Carolina Beach, North Carolina")

# Boundaries of locations listed in "place_names"
urban_boundaries <- sf::st_read("gis_data/merged_boundaries_coast.shp") %>%
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
    spin_wave(),
    h4("Loading...")
)

# Color palette for water level on site map
pal <- colorNumeric(
    palette = rev(brewer.pal(10,"RdBu")),
    domain = c(-3,3))

# Connect to database
con <- dbPool(
    drv =RPostgres::Postgres(),
    dbname = Sys.getenv("POSTGRESQL_DATABASE"),
    host = Sys.getenv("POSTGRESQL_HOST"),
    port = Sys.getenv("POSTGRESQL_PORT"),
    password = Sys.getenv("POSTGRESQL_PASSWORD"), 
    user = Sys.getenv("POSTGRESQL_USER") 
)

onStop(function() {
    poolClose(con)
})

database <- con %>% tbl("sensor_data")

# Update sensor locations with most recent data from database, also construct html popups
sensor_locations <- con %>% tbl("sensor_locations") %>%
    collect() %>%
    left_join(database %>%
                  group_by(sensor_ID) %>%
                  filter(date == max(date)),
              copy = T) %>%
    sf::st_as_sf(coords = c("lng", "lat"), crs = 4269) %>%
    mutate(
        html_popups = paste0(
            '<div align=\"center\">',
            '<h3>Site ',
            sensor_ID,
            '</h3>',
            '<h4>Current level: ',
            round(level, digits = 2),
            '</h4>',
            '<button id="view_site" type="button" class="btn btn-default action-button" onclick="{Shiny.onInputChange(&#39;view_site&#39;, (Math.random() * 1000) + 1);}">View data</button></div>'
        )
    )

#------------------------ Define UI ---------------------------------------
ui <- dashboardPage(
    # navbarPage(
    title = "SunnyD flooding project", 
    # id = "nav",
    skin = "black",
    header = dashboardHeader(
        title =  HTML('
            <a href="https://tarheels.live/sunnydayflood">
                <div width="300px">
                      <i class="fas fa-sun" role="presentation" aria-label="sun icon" style="color:#fbb040;position:absolute;left:15px;top:15px"></i><p style="color:white">Sunny Day Flooding Project</p>
                </div>
            </a>
                      '),
        titleWidth = 350
    ),
    sidebar = dashboardSidebar(
        width = 350,
        sidebarMenu(
            id = "nav",
            menuItem("Map", tabName = "Map", icon = icon("map")),
            
            conditionalPanel(
                condition = "input.nav === 'Map'",
                helpText("Select city from menu,", style = "color:white;font-size:12pt;text-align:center"),
                helpText("or click the location on the map",
                         style = "color:white;font-size:12pt;text-align:center"),
                
                #Select city, either Beaufort or Carolina Beach
                selectizeInput(
                    'city_name',
                    'Location',
                    choices = place_names,
                    options = list(
                        placeholder = 'Please select an option below',
                        onInitialize = I('function() { this.setValue(""); }')
                    )
                )
            ), 
            menuItem("Data", tabName = "Data", icon = icon("database")),
            
            conditionalPanel(
                condition = "input.nav === 'Data'",
                uiOutput("firstSelection", align = "center"),
                uiOutput("secondSelection", align = "center"),
                
                # filter by date, auto is the last three days of data
                dateRangeInput(
                    'dateRange',
                    label = 'Date range input: yyyy-mm-dd',
                    start = Sys.Date() - 3,
                    end = Sys.Date()
                )
            ), 
            
            menuItem("Pictures", tabName = "Pictures", icon = icon("camera"))
            )
        ),
    dashboardBody(
        fluidPage(
            useShinyalert(),
            
        tags$head(tags$style(HTML('
        .skin-black .main-header .logo {
          background-color: #13294B;
          border-right: 1px solid #13294B;
        }
        .skin-black .main-header .logo:hover {
          background-color: #000000;
        }
        
        .skin-black .main-header .navbar {
          background-color: #13294B;
        }
        
        .skin-black .main-header .navbar>.sidebar-toggle {
          color: #fff;
          border-right: 1px solid #13294B;
        }
        
        .skin-black .main-header .navbar .sidebar-toggle:hover {
          color: #fff;
          background: #000;
        }
        
        .main-header .sidebar-toggle {
          font-weight: 200; 
        }

      '))),
        tabItems(
    # Mapping tab for overview and site map
    tabItem(tabName = "Map",
        waiter::use_waiter(),
            fluidRow(
            # tags$style(type = "text/css", "#m {height: calc(100vh - 90px) !important;}"),
            leafletOutput(outputId = "m", width="100%",height="calc(100vh - 80px)"),
            style = "z-index: 5;" ## z-index modification
                    )
            ), 
    
    # Tab showing selected data and time series graphs
    tabItem(tabName = "Data",
            fluidRow(leafletOutput(outputId = "m2", height = "40vh")),
                 br(),
                 # Time series interactive plot or data table in separate tabs
            fluidRow(
                     tabsetPanel(
                     id = "data_tabs",
                     tabPanel(
                         "Plot",
                         plotlyOutput("site_level_ts", height = "40vh"),
                         uiOutput("site_data_panel")
                            ),
                     tabPanel(
                         "Table",
                          DT::dataTableOutput("site_data")
                            )
                                )
                    )
            ),
    tabItem(tabName = "Pictures",
            fluidRow(
                imageOutput(outputId = "camera", width = "20%", height = "100px")
                    )
            )
                )
                )
                )
                )



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Initialize wait screen for plotting data
    w <- Waiter$new(id="site_level_ts")
    shinyalert(title = "Welcome to the data viewer!",
               text = "Here you can view real-time data (fake data displayed now) from Sunny Day Flooding Project monitoring sites.",
               closeOnClickOutside = FALSE,
               showConfirmButton = T,
               confirmButtonCol = "#fbb040",
               type = "info",
               animation=F,
               size = "s")
    
    # If button on home page (View our sites) is clicked, will switch navbar tab to "Map"
    observeEvent(input$click, {
        updateTabItems(
        # updateNavbarPage(session,
            session = session,
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
            filter(sensor_ID %in% !!input$data_sensor) %>% 
            filter(date >= !!input$dateRange[1] & date <= !!input$dateRange[2]) %>% 
            collect()
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
                                            labelOptions = labelOptions(noHide = F)) %>% 
                                  setView(lng = map2_selected_location()[1], lat = map2_selected_location()[2], zoom=15)
                              )
    
    # If location is selected on Data tab dropdown, change the map view
    observeEvent(input$data_location, ignoreNULL = T,{
        leafletProxy(mapId = "m2") %>%
            setView(lng = map2_selected_location()[1], lat = map2_selected_location()[2], zoom=15)
    })
    
    # Create a red point for selected sensor
    observe({
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
        input$m2_marker_click
        })
    
    # Update reactive value for dropdown menu using map 2 marker click
    observe({
            reactive_selection$overall_data_sensor <- map2_clicked_sensor()
    })
    
    # # View site by switching tab
        observeEvent(input$view_site, {
            updateNavbarPage(session, 
                         inputId = "nav",
                         selected = "Data")
        })
    #       
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
        
        output$camera <- renderImage({
          invalidateLater(60*6*1000, session)
          tmpfile <- tempfile(fileext='.jpg')
          
          magick::image_read("/data/CAM_BF_01.jpg") %>% 
            magick::image_write(tmpfile)
            # magick::image_ggplot()
          
          list(src = tmpfile, 
               contentType = "image/jpeg",
               width = "800px",
               # height = "400px",
               alt = "This is alternate text")
        }, deleteFile = T)
}

# Run the application
shinyApp(ui = ui, server = server)
