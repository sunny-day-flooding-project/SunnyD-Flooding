# Packages to load
library(dplyr)
library(dbplyr)
library(lubridate)
library(shiny)
library(colourvalues)
library(waiter)
library(sf)
library(leaflet)
library(DT)
library(htmltools)
library(RColorBrewer)
library(highcharter)
library(magick)
library(xts)
library(RPostgres)
library(DBI)
library(pool)
library(shinyalert)
library(stringr)
library(shinydisconnect)
library(tippy)
library(httr)
library(shinyWidgets)
library(shinyjs)
library(bs4Dash)
library(foreach)
library(xml2)
library(tidyverse) 

# HTML waiting screen for initial load
waiting_screen <- tagList(
  spin_wave(),
  h4("Loading...")
)

# Color palette for water level on site map
pal <- colorNumeric(
  palette = rev(brewer.pal(10,"RdYlBu")),
  domain = c(-3.5,0.5))

pal_rev <- colorNumeric(
  palette = rev(brewer.pal(10,"RdYlBu")),
  domain = c(-3.5,0.5),
  reverse = T)

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

data_for_display <- con %>% 
  tbl("data_for_display") 

sensor_surveys <- con %>% 
  tbl("sensor_surveys") 

local_water_levels <- con %>% 
  tbl("api_data") %>%
  filter(type == "water_level")

fiman_gauge_key <- read_csv("fiman_gauge_key.csv")

global <- getOption("highcharter.global")
global$useUTC <- FALSE
global$timezoneOffset <- -300
options(highcharter.global = global)

noaa_wl <- function(id, type, begin_date, end_date){
  id <- as.character(id)
  
  request <-
      httr::GET(
        url = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter/",
        query = list(
          "station" = id,
          "begin_date" = begin_date %>% str_remove_all("-"),
          "end_date" = end_date %>% str_remove_all("-"),
          "product" = ifelse(type == "obs","water_level","predictions"),
          "units" = "english",
          "datum" = "NAVD",
          "time_zone" = "gmt",
          "format" = "json",
          "application" = "UNC_Institute_for_the_Environment, https://github.com/acgold"
        )
      )
    
    if(type == "obs") {
      wl <-
        tibble::as_tibble(jsonlite::fromJSON(rawToChar(request$content))$data)
      colnames(wl)[1:2] <- c("date", "level_ft_navd88")
    }
    
    if(type == "pred") {
      wl <-
        tibble::as_tibble(jsonlite::fromJSON(rawToChar(request$content))$predictions)
      colnames(wl)[1:2] <- c("date", "level_ft_navd88")
    }
    
    if(nrow(wl) == 0){
      stop("No data available")
    }
    
    wl <- wl %>%
      transmute(
        id = id,
        date = ymd_hm(date),
        level = as.numeric(level_ft_navd88),
        entity = ifelse(type == "obs", "NOAA Observed","NOAA Predictions"),
        notes = ifelse(type == "obs", "observation","prediction")
      )
    return(wl)
  }

fiman_wl <- function(site_id, begin_date, end_date){
  # station_keys <- fiman_gauge_key %>% 
  #   filter(site_id == id) %>% 
  #   filter(Sensor == "Water Elevation")
  
  # request <- httr::GET(url = Sys.getenv("FIMAN_URL"),
  #                      query = list(
  #                        "site_id" = station_keys$site_id,
  #                        "data_start" = paste0(format(begin_date, "%Y-%m-%d %H:%M:%S")),
  #                        "date_end" = paste0(format(end_date, "%Y-%m-%d %H:%M:%S")),
  #                        "format_datetime"="%Y-%m-%d %H:%M:%S",
  #                        "tz" = "UTC",
  #                        "show_raw" = T,
  #                        "show_quality" = T,
  #                        "sensor_id" =  station_keys$sensor_id
                         
  #                      ))
  
  # content <- request$content %>% 
  #   xml2::read_xml() %>% 
  #   xml2::as_list() %>% 
  #   as_tibble()
  
  # parsed_content <- content$onerain$response %>% 
  #   as_tibble() %>% 
  #   unnest_wider("general") %>% 
  #   unnest(cols = names(.)) %>% 
  #   unnest(cols = names(.)) %>% 
  #   mutate(data_time = lubridate::ymd_hms(data_time),
  #          data_value = as.numeric(data_value))
  
  # wl <- parsed_content %>%
  #   transmute(
  #     id = id,
  #     date = data_time,
  #     level = data_value,
  #     entity = "FIMAN",
  #     notes = "observation"
  #   )
  wl <- local_water_levels %>% 
      filter(id == site_id, date >= begin_date, date <= end_date) %>%
      select(id, date, value, api_name) %>%
      arrange(date) %>%
      transmute(
        id = id,
        date = date,
        level = value,
        entity = api_name,
        notes = "observation"
      ) %>%
      as_tibble()
  
  return(wl)
}

hohonu_wl <- function(site_id, begin_date, end_date){
  wl <- local_water_levels %>% 
      filter(id == site_id, date >= begin_date, date <= end_date) %>%
      select(id, date, value, api_name) %>%
      arrange(date) %>%
      transmute(
        id = id,
        date = date,
        level = value,
        entity = api_name,
        notes = "observation"
      ) %>%
      as_tibble()

  return(wl)
}

#wl_id, wl_src, wl_types, wl_url

get_local_wl <- function(wl_id, wl_src, type = c("obs"), begin_date, end_date) {
  # Each data source will have its own function called within this larger function
  switch(toupper(wl_src),
         "NOAA" = noaa_wl(id = wl_id,
                          type = type,
                          begin_date = begin_date,
                          end_date = end_date),
         "FIMAN" = fiman_wl(site_id = wl_id,
                            begin_date = begin_date,
                            end_date = end_date),
          "HOHONU" = hohonu_wl(site_id = wl_id,
                              begin_date = begin_date,
                              end_date = end_date)
  )
  
}

time_converter <- function(x){
  if(x < 60){
    return(paste0(strong(x)," minutes ago"))
  }
  else if(x >= 60 & x < 1440){
    return(paste0("about ", strong(round(x/60))," hour(s) ago"))
  }
  else(
    return(paste0("about ", strong(round(x/1440)), " day(s) ago"))
  )
}

jsCode <- "shinyjs.init = function() {
  $(document).on('shiny:sessioninitialized', function (e) {
  var mobile = window.matchMedia('only screen and (max-width: 768px)').matches;
  Shiny.onInputChange('is_mobile_device', mobile);
});
}"


#------------------------ Define UI ---------------------------------------
ui <- bs4Dash::dashboardPage(
  title = "Data Viewer", 
  header = bs4Dash::dashboardHeader(
    border = F,
    .list = list(span(tags$a(tags$img(src = "logo.svg", height = '50'), href = "https://tarheels.live/sunnydayflood/", target="_blank")
    ))
  ),
  sidebar = bs4Dash::dashboardSidebar(
    width = 400,
    skin = "light",
    status = "primary",
    elevation = 2,
    sidebarMenu(
      id = "nav",
      
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Data", tabName = "Data", icon = icon("database")),
      menuItem("Flood Cam", tabName = "Pictures", icon = icon("camera")),
      a(HTML('<li class="nav-item">
          <a class="nav-link" href="https://tarheels.live/sunny/" target="_blank">
            <i class="fas fa-external-link-alt nav-icon" role="presentation" aria-label="external-link-alt icon"></i>
            <p>Website
            <span class="right badge badge-success">Link</span></p>
          </a>
        </li>')
      ),
      a(HTML('<li class="nav-item">
                <a class="nav-link" href="http://eepurl.com/hM74xn" target="_blank">
                  <i class="fas fa-envelope nav-icon" role="presentation" aria-label="external-link-alt icon"></i>
                  <p>Sign Up for Flood Alerts</p>
                </a>
              </li>')
      ),
      # menuItem("Website", icon = icon("info-circle"), href = "https://tarheels.live/sunnydayflood/", newTab = T),
      menuItem("Sensors", tabName = "Sensors", icon = icon("microchip"),condition = "output.admin_login_status")
    )
  ),
  controlbar = bs4Dash::dashboardControlbar(
    controlbarItem( 
      br(),
      h2("Admin Login"),
      passwordInput(inputId = "admin_pswd", label = "Password"),
      actionButton(inputId = "admin_pswd_submit", label = "Log In", width = "100px"), 
      actionButton(inputId = "admin_logout", label = "Log Out", status = "secondary"), 
      br(),
      uiOutput(outputId = "admin_status")
    )
  ),
  body = bs4Dash::dashboardBody(
    fluidPage(
      disconnectMessage(
        text = "Your session has timed out! Try refreshing the page.",
        refresh = "Refresh",
        background = "#FFFFFF",
        colour = "#000000",
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 0.25,
        width = 450,
        top = "center",
        size = 24,
        css = ""
      ),
      useShinyjs(),
      extendShinyjs(text = jsCode, functions = c()),
      use_waiter(),
      waiter::waiter_show_on_load(html = spin_3k(),
                                  color = transparent(0)),
      tags$head(tags$link(rel = "shortcut icon", href = "https://tarheels.live/sunnydayflood/wp-content/uploads/sites/1319/2022/04/cropped-SDFP_sun-only_v3-01.png"),
                includeCSS("sunnyd-theme.css"),
                tags$style(HTML('
        
        .content-wrapper>.content {
          padding-top:15px !important;
          padding-bottom:15px !important;
          padding-left:0px !important;
          padding-right:0px !important;
        }
        
        .pretty-link {
          color: #7bafff;
        }
        
        .pretty-link:hover {
          color: #638ecf;
        }
        
        .navbar-white {
          background-color: #23272b !important; 
        }
        
        .navbar-dark {
          background-color: #23272b !important; 
        }

        .brand-link {
          background-color: #13294b !important;
          color: #fff !important;
          border-bottom: 1px solid black !important;
        }

        p {
          margin-bottom: 0px !important;
        }
        
        .dark-theme-icon fa fa-sun {
          color:white !important;
        }
        
        .fa, .fas {
          color:white !important;
        }
        
        th, td {
          padding: 7px;
        }
        
        td {
          text-align: center;
          vertical-align: middle;
        }

        tr {
          border-bottom: 1px solid #ddd;
        }
        
        .center {
          margin-left: auto;
          margin-right: auto;
        }

        #downloadData {
          background-color:#13294b;
          color:white !important;
          //font-size: 12pt;
          border:3px;
        }
        
        #downloadData:hover {
          background-color:#000000;
          color:white !important;
          //font-size: 12pt;
          border: 3px;
        }
        
        #get_plot_data {
          background-color: #fbb040;
          color: white;
          //font-size: 12pt;
          font-weight: bold;
          //border: 3px;
        }
        
        #get_plot_data:hover {
          background-color: #f1a93e;
          color: white;
          //font-size: 12pt;
          font-weight: bold;
          //border: 3px;
        }
        
        #refresh_button {
          background-color: #616C78;
          color: white;
          //font-size: 12pt;
          font-weight: bold;
          //border: 3px;
        }
        
        #refresh_button:hover {
          background-color: #40474f;
          color: white;
          //font-size: 12pt;
          font-weight: bold;
          //border: 3px;
        }
        
        #get_picture {
          background-color: #fbb040;
          color: white;
          font-size: 12pt;
          font-weight: bold;
          border: 3px;
        }
        
        #get_picture:hover {
          background-color: #f1a93e;
          color: white;
          font-size: 12pt;
          font-weight: bold;
          border: 3px;
        }
        
        #view_on_map_wl, #view_on_map_camera {
          background-color: #fbb040;
          color: white;
          font-weight: bold;
          border: 3px;
        }
        
        #view_on_map_wl:hover, #view_on_map_camera:hover {
          background-color: #f1a93e;
          color: white;
          font-weight: bold;
          border: 3px;
        }
        
        #camera img {max-width: 100%; max-height: 400px}
        
        .input-group-text {
          background-color: #ffffff00!important;
          border: none;
        }
        
        @media (min-width:992px) and (max-width: 1120px) {
          #guide img {
            max-width: 700px;
          }
        }

        @media (max-width:992px) {
          #guide img {
            max-width: 600px;
            height: auto;
          }
        }

        @media (max-width:670px) {
          #guide img {
            max-width: 400px;
            height: auto;
          }
        }
      '))),
      tabItems(
        
        # Mapping tab for overview and site map
        tabItem(tabName = "Map",
                fluidRow(
                  column(width = 3,
                         div(class="card",
                             div(class = "card-body",
                                 id = "instructions_box",
                                 p("Navigate to a study location and select a map layer. Hover and click on points explore live data and pictures"),
                                 br(),
                                 selectInput(
                                   'city_name',
                                   label = h4("Location"),
                                   choices = c(""),
                                   selected = NULL,
                                   multiple = F,
                                   selectize = F
                                 ),
                                 radioButtons("map_layers", label = h4("Map Layers"),
                                              choices = list("Water Level Sensors" = 1, "Flood Cams" = 2), 
                                              selected = 2)
                             )
                         )
                  ),
                  column(width = 9,
                         div(class="card",
                             div(class = "card-body",
                                 style="padding:0px;",
                                 uiOutput(outputId = "leaf")
                             )
                         )
                  )
                )
                
        ), 
        
        # Tab showing selected data and time series graphs
        tabItem(tabName = "Data",
                
                # Add a banner if a sensor site is "under construction"
                # uiOutput("construction_banner"),
                
                # Time series interactive plot or data table in separate tabs
                box(width=12, 
                    id = "flood_status", 
                    collapsed = T,
                    #   p(
                    #   icon("info-circle"),
                    #   "  Flood Status: ",
                    #   strong("Loading, ", style = "color:white;"),
                    #   style = "margin-bottom: 0px;display:inline;"
                    # ),
                    # status = "gray",
                    solidHeader = T,
                    p(strong("NOT FLOODING:"), " Water level measurements within this storm drain indicate that water is", em("likely not near the road surface."), align = "left",style="margin-bottom:0px;"),
                    p(strong("WARNING:"), " Water level measurements within this storm drain indicate that water is", em("likely near the road surface."), align = "left",style="margin-bottom:0px;"),
                    p(strong("FLOODING:"), " Water level measurements within this storm drain indicate that water is", em("likely on the road surface."),"This status is triggered by a loss of communications with our underground sensor (meaning that our sensor is completely under water and can't transmit data)", align = "left",style="margin-bottom:0px;"),
                    p(strong("UNKNOWN:"), " Water level within this storm drain is unknown because the sensor has not reported water level recently.", align = "left",style="margin-bottom:0px;")
                ),
                
                
                
                tabBox(width=12,
                       id = "data_tabs",
                       type = "tabs",
                       collapsible = F,
                       tabPanel(
                         "Plot",
                         highchartOutput("site_level_ts", width = "100%"),
                         p("* no water can be detected beneath sensor elevation line"),
                         hr(),
                         h4("Plot options"),
                         fluidRow(
                           column(width = 4,
                                  uiOutput("firstSelection"),
                                  uiOutput("secondSelection")
                           ),
                          #  column(width=1),
                           column(width = 4,
                                  selectInput(inputId = "elev_datum", label = "Elevation Datum", selectize = F,choices = c("Road","NAVD88"), selected = "Road"),
                                  p(strong("Local water levels"),tippy(icon("info-circle",style="font-size:14px"), h5("Click the button below to add nearby downstream water levels to the plot.",br(),br(),"Adding these data can help visualize when flooding may occur.",br(),br(),"Turning this option on may slow down the drawing of the plot.",style = "text-align:left;"))),
                                  materialSwitch(inputId = "view_3rdparty_data", label = ,value = F,inline=T, status = "success"),
                                  uiOutput(outputId="thirdparty_info", style="display:inline;"),
                                  br(),
                                  materialSwitch(inputId = "view_alt_3rdparty_data", label = ,value = F,inline=T, status = "success"),
                                  uiOutput(outputId="alt_thirdparty_info", style="display:inline;")
                           ),
                          #  column(width=1),
                           column(width = 4,
                                  uiOutput("dateRange"),
                                  br(),
                                  div(
                                    actionButton("get_plot_data", "Plot new dates"),
                                    actionButton("refresh_button", "Refresh", status = "primary"),
                                    style="text-align:center", size = "lg")
                           ),
                          #  column(width=1)
                           
                         ),
                         hr(),
                         fluidRow(
                           column(width=12,
                            uiOutput("site_notes")
                           )
                         )
                       ),
                      #  tabPanel(
                      #    "Site Info",
                      #    uiOutput(outputId = "site_description")
                      #  ),
                       tabPanel(
                         "Download",
                         h3("Click the button below to download the selected data.", align="center"),
                         br(),
                         p("Data will be downloaded as a .csv file named",strong("site name_minimum date_maximum date.csv"),align="center"),
                         br(),
                         div(downloadButton(outputId = "downloadData", label = "Download Selected Data", class = "download_button", icon = icon("download")), align="center")
                       ),
                       tabPanel(
                        "How to understand this plot",
                        fluidRow(
                          column(width=12,
                            uiOutput("guide_text"),
                            hr()
                           ),
                           column(width=12,
                            imageOutput(outputId = "guide", width = "100%")
                           )
                         )
                       )
                )
                
        ),
        tabItem(tabName = "Pictures",
                # tabBox(
                #   collapsible = F,
                #   type = "tabs",
                #   tabPanel(
                #     "Pictures",
                #     imageOutput(outputId = "camera"),
                #     uiOutput("firstCameraSelection", align = "center"),
                #     uiOutput("secondCameraSelection", align = "center")
                #   ),
                #   # column(6,
                #   #        h1("Water detected?",tippy(icon("info-circle",style="font-size:16px"), tooltip = div(h5("We are using machine learning to detect water on the road surface using our flood cams.",br(),br(),"The model used here is trained to distinguish pictures that have water in them from those that do not. Cars, salt water stains, sun glare, and other features in the photos can decrease the accuracy of the model.",br(),br(),"This model is still in development and is for informational purposes only."), style = "text-align:left"))),
                #   #        div(p("In Development",style="color:white;text-align: center"),style="background-color:#fbb040;width:125px;border-radius: 20px"),
                #   #        # p("The chart below shows the probability that water has been detected in this picture with our machine learning model"),
                #   #         uiOutput(outputId = "camml")))
                #   #        # highchartOutput(outputId ="tf_predict")))
                #   # ),
                #   tabPanel(
                #     "Site Info",
                #     h3("Site description coming soon"),
                #     actionButton("view_on_map_camera", label = "View site on map", icon = icon("map"))
                    
                #   )
                  
                # )
                fluidRow(
                    column(width = 6,
                          div(class="card",
                              div(class = "card-body",
                                  imageOutput(outputId = "camera"),
                                  uiOutput("firstCameraSelection", align = "center"),
                                  uiOutput("secondCameraSelection", align = "center")
                              )
                          )
                    ),
                )
        ),
        tabItem(tabName = "Sensors",
                uiOutput("dashboard_panels")
                )
      )
    )
  ),
  footer = dashboardFooter(
    left = p(em(strong("Disclaimer: "), "Data are preliminary and for informational use only.")),
    right =  paste("Copyright", format(Sys.Date(), "%Y"), "Sunny Day Flooding", sep=" ")
  )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Initialize wait screen for plotting data
  w <- Waiter$new(id="site_level_ts",
                  html = spin_3k(),
                  color = transparent(.75))   
  
  # Initialize wait screen for image rendering
  w2 <- Waiter$new(id="camera",
                   html = spin_3k(),
                   color = transparent(.75)) 

  # Initialize wait screen for plot guide rendering
  w3 <- Waiter$new(id="guide",
                   html = spin_3k(),
                   color = transparent(.75)) 
  
  w_admin <- Waiter$new(id="admin_pswd_submit",
                   html = spin_3k(),
                   color = transparent(.75)
                   ) 
  
  output$dateRange <- renderUI({
    dateRangeInput(
      inputId = 'dateRange',
      label = 'Date range',
      start = Sys.Date() - 2,
      end = Sys.Date() + 1,
      max = Sys.Date() + 14
      )
  })

  # Setup date filter for Data tab sidebar UI
  output$date_filter_photos <- renderUI(
    dateRangeInput(
      'dateRangePhotos',
      label = 'Date range input: yyyy-mm-dd',
      start = Sys.Date() - 2,
      end = Sys.Date() + 1
    )
  )
  
  # Popup on load to display info
  shinyalert(text = includeMarkdown("landing_text.md"),
             html = T,
             closeOnClickOutside = FALSE,
             showConfirmButton = T,
             confirmButtonCol = "#fbb040",
             confirmButtonText = "OK",
             type = "info",
             animation=F,
             size = "s")
  
  
  output$leaf = renderUI({
    req(!is.null(input$is_mobile_device))
    
    
    if (input$is_mobile_device == F) {
      leafletOutput(outputId = "m",
                    width = "100%",
                    height = "calc(100vh - 160px)")
    }
    
    else if (input$is_mobile_device == T) {
      leafletOutput(outputId = "m",
                    width = "100%",
                    height = "calc(100vh - 130px)")
    }
    
  })
  
  observeEvent(input$nav,
               {
                 req(input$is_mobile_device == T)
                 # for desktop browsers
                 addClass(selector = "body", class = "sidebar-collapse")
                 # for mobile browsers
                 removeClass(selector = "body", class = "sidebar-open")
               })
  
  admin_login_status <- reactiveVal(value = F)
  
  output$admin_status <- renderUI(
    span(p("Status: ",style="display:inline-block;font-weight: bold;"), p("Logged out", style="display:inline-block;")) 
  )
  
  # Hide alternate 3rd party data input by default
  shinyjs::hide(id = "view_alt_3rdparty_data")

  output$admin_login_status <- reactive({admin_login_status()})

  outputOptions(output, "admin_login_status", suspendWhenHidden = FALSE)
  
  observeEvent(input$admin_pswd_submit, {
    req(input$admin_pswd, admin_login_status() == F)
    
    w_admin$show()
    
    if(isolate(input$admin_pswd) == Sys.getenv("ADMIN_PSWD")){
      
      admin_login_status(T)
      
      updateActionButton(inputId = "admin_pswd_submit",
                         icon = tags$i(
                           class = "fas fa-check-circle", 
                           style = "color: rgb(0,166,90)"
                         ))
      
      toast(
        title = "🎉 Logged in! 🎉",
        options = list(
          autohide = TRUE,
          # icon = "fas fa-check",
          close = FALSE,
          delay = 3000,
          position = "topRight"
        )
      )
      
      output$admin_status <- renderUI(
        span(p("Status: ",style="display:inline-block;font-weight: bold;"), p("Logged in", style="display:inline-block;")) 
      )
      
      w_admin$hide()
    }
    
    if(isolate(input$admin_pswd) != Sys.getenv("ADMIN_PSWD")){
      
      w_admin$show()
      
      updateActionButton(inputId = "admin_pswd_submit",
                         icon = tags$i(
                           class = "fas fa-times-circle", 
                           style = "color: red"
                         ))
      
      toast(
        title = "Incorrect password!",
        options = list(
          autohide = TRUE,
          icon = "fas fa-times-circle",
          close = FALSE,
          delay = 3000,
          position = "topRight"
        )
      )
      
      output$admin_status <- renderUI(
        span(p("Status: ",style="display:inline-block;font-weight: bold;"), p("Logged out", style="display:inline-block;")) 
      )
      
      w_admin$hide()
    }
  })
  
  observeEvent(input$admin_logout, {
    req(isolate(admin_login_status() == T))
    
    w_admin$show()
    
    admin_login_status(F)
    
    updateActionButton(inputId = "admin_pswd_submit",
                       icon = tags$i(
                         class = "fas fa-times-circle", 
                         style = "color: red"
                       ))
    
    toast(
      title = "Logged out!",
      options = list(
        autohide = TRUE,
        icon = "fas fa-times-circle",
        close = FALSE,
        delay = 3000,
        position = "topRight"
      )
    )
    
    output$admin_status <- renderUI(
      span(p("Status: ",style="display:inline-block;font-weight: bold;"), p("Logged out", style="display:inline-block;")) 
    )
    
    w_admin$hide()
  }
  )
  
  # Load Data
  # Update sensor locations with most recent data from database
  # Get data from last 90 days only to speed up load time
  start.date <- Sys.Date() - 90
  tmp <- con %>% 
    tbl("data_for_display") %>%
    filter(date > start.date)
  sensor_locations <- tmp %>%
                group_by(sensor_ID) %>%
                filter(date == max(date, na.rm=T)) %>% 
                collect() %>% 
                mutate(sensor_water_level = sensor_water_level_adj,
                       road_water_level = road_water_level_adj) %>% 
    mutate(date_lst = lubridate::with_tz(date, tzone = "America/New_York")) %>% 
    filter(!is.na(lat) & !is.na(lng)) %>% 
    sf::st_as_sf(coords = c("lng", "lat"), crs = 4269) 

    sensor_labels <- con %>% 
                    tbl("sensor_surveys") %>% 
                    group_by(sensor_ID) %>%
                    filter(date_surveyed == max(date_surveyed, na.rm=T)) %>% 
                    select(sensor_ID, sensor_label) %>% 
                    distinct(sensor_ID, .keep_all = TRUE) %>% 
                    collect()

    sensor_locations <- merge(sensor_locations, sensor_labels, by = c("sensor_ID")) %>% 
      mutate(
        html_label = paste0(sensor_label, "<br> (", sensor_ID, ")"),
        sensor_label = paste0(sensor_label, " (", sensor_ID, ")")
      ) %>%                
      mutate(
        html_popups = paste0( 
          '<div>',
          '<h4 align="center"><strong>',html_label,'</h4></strong>',
          '<h5 align="center">Last level:</h5>',
          '<h4 align="center">',round(road_water_level_adj, digits = 2),'</h4>',
          '<p align="center">',paste0(format(date_lst, "%I:%M %p", usetz = T)," - ",format(date_lst, "%b %d, %Y")),'</p>',
          '<p align="center">Click to view data at this site</p>',
          '</div>'
        ),
      )
  
  camera_locations <- con %>% 
    tbl("camera_locations") %>%
    # filter(camera_ID != 'CAM_DE_02') %>%
    collect() %>% 
    left_join(con %>% 
                tbl("photo_info") %>% 
                group_by(camera_ID) %>% 
                filter(DateTimeOriginalUTC == max(DateTimeOriginalUTC, na.rm=T)) %>% 
                collect(), by = c("camera_ID")) %>% 
    mutate(date_lst = lubridate::with_tz(DateTimeOriginalUTC , tzone = "America/New_York")) %>% 
    sf::st_as_sf(coords = c("lng", "lat"), crs = 4269) %>% 
    mutate(
      html_label = paste0(camera_label, "<br> (", camera_ID, ")"),
      camera_label = paste0(camera_label, " (", camera_ID, ")")
    ) %>%
    mutate(
      html_popups = paste0( 
        '<div>',
        '<h4 align="center"><strong>',html_label,'</h4></strong>',
        '<h5 align="center">Last picture:</h5>',
        '<p align="center">',paste0(format(date_lst, "%I:%M %p", usetz = T)," - ",format(date_lst, "%b %d, %Y")),'</p>',
        '<p align="center">Click to view this camera</p>',
        '</div>'
      )
    )
  
  reference_elevations <- con %>%
    tbl("sensor_surveys") %>%
    group_by(sensor_ID) %>% 
    select(sensor_ID, reference_elevation, reference_elevation_type) %>%
    distinct() %>%
    collect()

  # Labels for sensor map 1
  sensor_locations_labels <- as.list(sensor_locations$html_popups)
  
  # Labels for sensor map 1 camera
  camera_locations_labels <- as.list(camera_locations$html_popups)

  city_name_options <- unique(c(sensor_locations$place, camera_locations$place))
  
  updateSelectInput(session = session,
                    inputId = "city_name",
                    # label = h4("Location"),
                    choices = c("", city_name_options),
                    selected = NULL)
  
  
  # Set the reactive values that will be updated by user inputs
  reactive_selection <- reactiveValues(overall_data_sensor = sensor_locations$sensor_ID[1],
                                       overall_data_location = unique(sensor_locations$place)[1],
                                       overall_data_sensor_choices = sensor_locations$sensor_ID[sensor_locations$place == unique(sensor_locations$place)[1]],
                                       
                                       overall_camera_ID = camera_locations$camera_ID[1],
                                       overall_camera_location = unique(camera_locations$place)[1],
                                       overall_camera_choices = camera_locations$camera_ID[camera_locations$place == unique(camera_locations$place)[1]]
  )
  
  reactive_min_date <- reactiveVal()
  reactive_min_date((Sys.Date() - 2) %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
  reactive_max_date <- reactiveVal()
  reactive_max_date((Sys.Date() + 1) %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
  
  observeEvent(input$get_plot_data,{
    data_n <- 10 * 24 * (input$dateRange[2] - input$dateRange[1])
    admin_login_nonreactive <- admin_login_status()
    
    if(data_n > 7440){
      
      if(admin_login_nonreactive == F){
      shinyalert::shinyalert(type="error", 
                             title="Data request is too large",
                             text="Please select a shorter time span. Maximum request is 30 days .")
      }
      
      if(admin_login_nonreactive == T){
        reactive_min_date(input$dateRange[1] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
        reactive_max_date(input$dateRange[2] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
      }
    }
    if(data_n <= 7440){
      reactive_min_date(input$dateRange[1] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
      reactive_max_date(input$dateRange[2] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
    }
  })
  

  # Reactive value that stores the data from the selected sensor site, filtered by date range
  sensor_data <- reactive({
    req(input$data_sensor)
    
    input$refresh_button
    
    data_for_display %>%
      filter(sensor_ID %in% !!input$data_sensor) %>%
      filter(date >= !!reactive_min_date() & date < !!reactive_max_date(),
             qa_qc_flag == F) %>% 
      collect() 
    
  })
  
  
  observeEvent(input$refresh_button,{
    toast(
      title = "Refreshed!",
      options = list(
        autohide = TRUE,
        icon = "fas fa-check",
        close = FALSE,
        delay = 3000,
        position = "bottomRight"
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$data_sensor, "_",format(reactive_min_date(),"%Y%m%d"), "_",format(reactive_max_date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      downloadData = sensor_data() %>% 
        select(date, sensor_elevation, road_elevation, sensor_water_level_adj, road_water_level_adj) %>%
        arrange(date) %>%
        mutate(
          date = format(date, '%m/%d/%Y %H:%M'),
          sensor_elevation = round(sensor_elevation, digits=2),
          road_elevation = round(road_elevation, digits=2),
          sensor_water_level_adj = round(sensor_water_level_adj, digits=2),
          road_water_level_adj = round(road_water_level_adj, digits=2),
        )

      headers <- c('timestamp (UTC)','sensor elev. (ft NAVD88)','road elev. (ft NAVD88)','water level (ft NAVD88)','water level (ft above or below road elev.)')
      colnames(downloadData) <- headers
      write_csv(downloadData, file)
    }
    # content = function(file) {
    #   write.csv(sensor_data() |> arrange(date), file)
    # }
  )
  
  # Input drop-downs menus for data page
  output$firstSelection <- renderUI({
    selectInput("data_location", "Location", choices = unique(sensor_locations$place), selected = reactive_selection$overall_data_location, selectize = F)
  })
  
  output$secondSelection <- renderUI({
    selectInput("data_sensor", "Sensor", choices = reactive_selection$overall_data_sensor_choices, selected = reactive_selection$overall_data_sensor, selectize = F,multiple = F)
  })
  
  output$firstCameraSelection <- renderUI({
    selectInput("camera_location", "Location", choices = unique(camera_locations$place), selected = reactive_selection$overall_camera_location, selectize = F)
  })
  
  output$secondCameraSelection <- renderUI({
    selectInput("camera_ID", "Camera", choices = reactive_selection$overall_camera_choices, selected = reactive_selection$overall_camera_ID, selectize = F,multiple = F)
  })
  
  map_flood_status_reactive <- reactive({
    input$refresh_button

    return(
      data_for_display %>% 
        group_by(sensor_ID) %>%
        slice_max(n = 3, order_by = date) %>%
        collect() %>%
        mutate(
          sample_interval = difftime(lag(date),date, units = "mins"),
          min_interval = as.numeric(min(sample_interval, na.rm = T)),
          current_time = Sys.time() %>% lubridate::with_tz(tzone = "UTC")
        ) %>%
        filter(date == max(date, na.rm = T)) %>%
        mutate(
          above_alert_wl = sensor_water_level_adj >= alert_threshold,
          time_since_measurement = as.numeric(floor(difftime(current_time,date, unit = "mins"))),
          time_since_measurement_text = time_converter(time_since_measurement),
          is_comms_down = time_since_measurement >= 180,
          is_current = (time_since_measurement <= 40), #(min_interval + 10 + 10 + 3)
          flood_status = ifelse(above_alert_wl & !is_current & !is_comms_down, "FLOODING", 
                                ifelse(above_alert_wl & is_current, "WARNING", 
                                       ifelse(!above_alert_wl & is_current, "NOT FLOODING",
                                              "UNKNOWN"))) 
        ) %>% 
        dplyr::select(sensor_ID, time_since_measurement, time_since_measurement_text, flood_status)
    )
  })
  
  flood_status_reactive <- reactive({
    req(input$data_sensor)
    input$refresh_button
    
    return(
      data_for_display %>%
        filter(sensor_ID == !!input$data_sensor) %>%
        slice_max(n = 3, order_by = date) %>%
        collect() %>%
        mutate(
          sample_interval = lag(date) - date,
          min_interval = min(sample_interval, na.rm = T),
          current_time = Sys.time() %>% lubridate::with_tz(tzone = "UTC")
        ) %>%
        filter(date == max(date, na.rm = T)) %>%
        mutate(
          above_alert_wl = sensor_water_level_adj >= alert_threshold,
          time_since_measurement = floor(difftime(current_time,date, unit = "mins")),
          time_since_measurement_text = time_converter(time_since_measurement),
          is_comms_down = time_since_measurement >= 180,
          is_current = (time_since_measurement <= 40), #(min_interval + 10 + 10 + 3)
          flood_status = ifelse(above_alert_wl & !is_current & !is_comms_down, "FLOODING", 
                                ifelse(above_alert_wl & is_current, "WARNING", 
                                       ifelse(!above_alert_wl & is_current, "NOT FLOODING",
                                              "UNKNOWN")))
        )
    )
  })
  
  
  # showing the banner of flood alert status
  observe({
    flood_status_df <- flood_status_reactive()
    
    if(flood_status_df$flood_status == "NOT FLOODING"){
        updateBox(id = "flood_status",
                  action="update",
                  session = session,
                  options = list(
                    title = p(
                      icon("info-circle"),
                      "  Roadway Flood Status: ",
                      strong("NOT FLOODING, ", style = "color:white;"),
                      HTML(flood_status_df$time_since_measurement_text),
                      style = "margin-bottom: 0px;display:inline;"
                    ),
                    status = "success",
                    solidHeader = T
                  )
        )
      }
      
      else if(flood_status_df$flood_status == "UNKNOWN"){
        updateBox(id = "flood_status",
                  action="update",
                  session = session,
                  options = list(
                    title = p(
                      icon("info-circle"),
                      "  Roadway Flood Status: ",
                      strong("UNKNOWN, ", style = "color:white;"),
                      HTML(flood_status_df$time_since_measurement_text),
                      style = "margin-bottom: 0px;display:inline;"
                    ),
                    status = "gray",
                    solidHeader = T
                  )
        )
    }
    
    else if(flood_status_df$flood_status == "WARNING"){
        updateBox(id = "flood_status",
                  action="update",
                  session = session,
                  options = list(
                    title = p(
                      icon("info-circle"),
                      "  Roadway Flood Status: ",
                      strong("WARNING", style = "color:white;"),
                      HTML(flood_status_df$time_since_measurement_text),
                      style = "margin-bottom: 0px;display:inline;"
                    ),
                    status = "warning",
                    solidHeader = T
                  )
        )
      }
      
      else if(flood_status_df$flood_status == "FLOODING"){
        updateBox(id = "flood_status",
                  action="update",
                  session = session,
                  options = list(
                    title = p(
                      icon("info-circle"),
                      "  Roadway Flood Status: ",
                      strong("FLOODING, ", style = "color:white;"),
                      HTML(flood_status_df$time_since_measurement_text),
                      style = "margin-bottom: 0px;display:inline;"
                    ),
                    status = "danger",
                    solidHeader = T
                  )
        )
    }
  })
  
  
  # Create initial map for "map" tab 
  
  output$m <-renderLeaflet(leaflet() %>%
                             addProviderTiles(group = "Positron (default)",provider = providers$CartoDB.Positron) %>%
                             addProviderTiles(group = "Dark Matter",provider = providers$CartoDB.DarkMatter) %>%
                             addProviderTiles(group = "Imagery",provider = providers$Esri.WorldImagery) %>%
                             addTiles(group = "OSM") %>%
                             setView(lng = -77.360784, lat = 34.576053, zoom = 8) %>%
                            #  addCircleMarkers(data = sensor_locations %>% 
                            #                     left_join(isolate(map_flood_status_reactive()), by = "sensor_ID"), group = "sensor_site",
                            #                   # popup = ~html_popups,
                            #                   label = lapply(sensor_locations_labels,HTML),
                            #                   labelOptions = labelOptions(direction = "top", style=list("border-radius" = "10px")),
                            #                   # clusterOptions = markerClusterOptions(),
                            #                   # clusterId = "place",
                            #                   layerId = sensor_locations$sensor_ID,
                            #                   color = "black",
                            #                   fillColor = sensor_locations %>%
                            #                     left_join(isolate(map_flood_status_reactive()), by = "sensor_ID") %>%
                            #                     left_join(tibble("flood_status" = c("UNKNOWN","FLOODING", "WARNING", "NOT FLOODING"),
                            #                                      "flood_color" = c("grey", "#dc3545", "#ffc107", "#28a745")), by = "flood_status") %>% 
                            #                     pull(flood_color),
                            #                   fillOpacity = 1) %>% 
                            addCircleMarkers(data = camera_locations,
                                group = "camera_site",
                                label = lapply(camera_locations_labels,HTML),
                                labelOptions = labelOptions(direction = "top", style=list("border-radius" = "10px")),
                                layerId = camera_locations$camera_ID,
                                color = "black",
                                fillColor = camera_locations %>%
                                  left_join(isolate(map_flood_status_reactive()), by = "sensor_ID") %>%
                                  left_join(tibble("flood_status" = c("UNKNOWN","FLOODING", "WARNING", "NOT FLOODING", NA),
                                                    "flood_color" = c("grey", "#dc3545", "#ffc107", "#28a745", "grey")), by = "flood_status") %>% 
                                  pull(flood_color),
                                fillOpacity = 1.0
                            ) %>%
                             # leaflet::addLegend('bottomright', pal = pal_rev, values = c(-3.5,0.5),
                             #                    title = 'Water level<br>relative to<br>surface (ft)',
                             #                    opacity = 1,
                             #                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
                             leaflet::addLegend('bottomright',
                                                title = "Flood status",
                                                colors = c("grey","#dc3545","#ffc107","#28a745"),
                                                labels = c("Unknown","Flooding", "Warning", "Not Flooding"),
                                                opacity = 1) %>% 
                             addLayersControl(
                               baseGroups = c("Positron (default)", "Dark Matter","Imagery", "OSM"),
                               options = layersControlOptions(collapsed = T)) %>% 
                             addEasyButton(easyButton(
                               icon="fa-globe",title="Zoom out",
                               onClick=JS("function(btn, map){ map.setView({lng:-77.360784, lat:34.576053}, 8); }"),
                               position = "topright")) 
  )
  
  # reactive value storing the mean coordinates of the sensors from the city selected from the map tab
  map1_selected_location <- reactive({
    req(input$city_name)
    reactive_selection$overall_data_location <- input$city_name
    reactive_selection$overall_camera_location <- input$city_name
    
    sensors = sensor_locations %>% 
              filter(place == input$city_name)
    if(nrow(sensors) > 0) {
      sensor_locations %>% 
        filter(place == input$city_name) %>% 
        sf::st_coordinates() %>% 
        as_tibble() %>% 
        summarise(lng = mean(X, na.rm=T),
                  lat = mean(Y, na.rm=T))
    } else {
      camera_locations %>% 
        filter(place == input$city_name) %>% 
        sf::st_coordinates() %>% 
        as_tibble() %>% 
        summarise(lng = mean(X, na.rm=T),
                  lat = mean(Y, na.rm=T))
    }
  })
  
  # If city name is selected on the map tab, fly to the location
  observeEvent(input$city_name,{
    
    if(input$city_name == ""){
      leafletProxy(mapId = "m") %>% 
        setView(lng = -77.360784, lat = 34.576053, zoom = 8)
    }
    
    if(input$city_name != ""){
      # Down East and Hawaii have sensors that are spread further apart than other locations so use a smaller zoom value
      # This should get moved into a database table if additional sites are added that need a different zoom level
      if (input$city_name == 'Down East, North Carolina') {
        zoom <- 11
      } else if (input$city_name == 'Hawaii') {
        zoom <- 11
      } else {
        zoom <- 16
      }
      leafletProxy(mapId = "m") %>% 
        setView(lng = map1_selected_location()[1], lat = map1_selected_location()[2], zoom=zoom)
    }
  })
  
  
  observeEvent(input$view_on_map_wl,{
    updateNavbarPage(session, 
                     inputId = "nav",
                     selected = "Map")
    
    sensor_location_selected <- sensor_locations %>% 
      filter(sensor_ID == input$data_sensor) 
    
    sensor_location_selected_coords <- sensor_location_selected %>% 
      sf::st_coordinates()
    
    sensor_location_selected_label <-  as.list(str_remove(sensor_location_selected$html_popups,"<p align=\"center\">Click to view data at this site</p>"))
    
    leafletProxy(mapId = "m") %>%
      setView(lng = sensor_location_selected_coords[1], lat = sensor_location_selected_coords[2], zoom = 18) %>% 
      addPopups(lng = sensor_location_selected_coords[1], lat = sensor_location_selected_coords[2], lapply(sensor_location_selected_label,HTML),
                options = popupOptions(closeButton = T, closeOnClick = T)
      )
    
    updateRadioButtons(session, 
                       inputId = "map_layers", 
                       choices = list("Water Level Sensors" = 1, "Flood Cams" = 2), 
                       selected = 1)
    
  })
  
  # Use this for cameras
  observeEvent(input$view_on_map_camera,{
    updateNavbarPage(session,
                     inputId = "nav",
                     selected = "Map")
    
    camera_location_selected <- camera_locations %>%
      filter(camera_ID == input$camera_ID) 
    
    camera_location_selected_coords <- camera_location_selected %>% 
      sf::st_coordinates()
    
    camera_location_selected_label <-  as.list(str_remove(camera_location_selected$html_popups,"<p align=\"center\">Click to view this camera</p>"))
    
    leafletProxy(mapId = "m") %>%
      setView(lng = camera_location_selected_coords[1], lat = camera_location_selected_coords[2], zoom = 18) %>%
      addPopups(lng = camera_location_selected_coords[1], lat = camera_location_selected_coords[2], camera_location_selected_label,
                options = popupOptions(closeButton = T, closeOnClick = T)
      )
    
    updateRadioButtons(session, 
                       inputId = "map_layers", 
                       choices = list("Water Level Sensors" = 1, "Flood Cams" = 2), 
                       selected = 2)
    
  })
  
  # Make camera icon for camera layer. Can be moved somewhere better
  map_icon <- awesomeIcons(
    icon = 'fa-camera',
    iconColor = '#FFFFFF',
    library = 'fa'
  )
  
  # Toggle layers on map
  observeEvent(input$map_layers,{
    if(input$map_layers == 1){
      leafletProxy(mapId = "m") %>% 
        clearGroup("camera_site") %>% 
        addCircleMarkers(data = sensor_locations %>% 
                           left_join(isolate(map_flood_status_reactive()), by = "sensor_ID"), group = "sensor_site",
                         # popup = ~html_popups,
                         label = lapply(sensor_locations_labels,HTML),
                         labelOptions = labelOptions(direction = "top", style=list("border-radius" = "10px")),
                         # clusterOptions = markerClusterOptions(),
                         # clusterId = "place",
                         layerId = sensor_locations$sensor_ID,
                         color = "black",
                         fillColor = sensor_locations %>%
                           left_join(isolate(map_flood_status_reactive()), by = "sensor_ID") %>%
                           left_join(tibble("flood_status" = c("UNKNOWN","FLOODING", "WARNING", "NOT FLOODING"),
                                            "flood_color" = c("grey", "#dc3545", "#ffc107", "#28a745")), by = "flood_status") %>% 
                           pull(flood_color),
                         fillOpacity = 1) 
    }
    
    if(input$map_layers == 2){
      leafletProxy(mapId = "m") %>% 
        clearGroup("sensor_site") %>% 
        # addAwesomeMarkers(data = camera_locations, icon=map_icon, group = "camera_site",
        #                   label = lapply(camera_locations_labels,HTML),
        #                   labelOptions = labelOptions(direction = "top", style=list("border-radius" = "10px")),
        #                   # clusterOptions = markerClusterOptions(),
        #                   # clusterId = "place",
        #                   layerId = camera_locations$camera_ID,
        # )
        addCircleMarkers(data = camera_locations,
                           group = "camera_site",
                         label = lapply(camera_locations_labels,HTML),
                         labelOptions = labelOptions(direction = "top", style=list("border-radius" = "10px")),
                         layerId = camera_locations$camera_ID,
                         color = "black",
                         fillColor = camera_locations %>%
                            left_join(isolate(map_flood_status_reactive()), by = "sensor_ID") %>%
                            left_join(tibble("flood_status" = c("UNKNOWN","FLOODING", "WARNING", "NOT FLOODING", NA),
                                              "flood_color" = c("grey", "#dc3545", "#ffc107", "#28a745", "grey")), by = "flood_status") %>% 
                            pull(flood_color),
                         fillOpacity = 1.0
        ) 
      
    }
  })
  
  # Updates the choices for the sensor ID on the data tab
  observe({
    ids <- sensor_locations$sensor_ID[sensor_locations$place == input$data_location]
    choices = setNames(as.list(ids), sensor_locations$sensor_label[sensor_locations$place == input$data_location])
    reactive_selection$overall_data_sensor_choices <- choices
  })
  
  # Updates the choices for the camera ID on the data tab
  observe({
    ids <- camera_locations$camera_ID[camera_locations$place == input$camera_location] 
    choices = setNames(as.list(ids), camera_locations$camera_label[camera_locations$place == input$camera_location])
    reactive_selection$overall_camera_choices <- choices
  })
  

  observeEvent(input$m_marker_click,{

    if(input$map_layers == 1){
      reactive_selection$overall_data_sensor <- input$m_marker_click
      reactive_selection$overall_data_location <- unique(sensor_locations$place[sensor_locations$sensor_ID == input$m_marker_click$id])
      reactive_selection$overall_camera_location <- unique(sensor_locations$place[sensor_locations$sensor_ID == input$m_marker_click$id])

      updateNavbarPage(session,
                       inputId = "nav",
                       selected = "Data")
    }

    if(input$map_layers == 2){
        reactive_selection$overall_camera_location <- unique(camera_locations$place[camera_locations$camera_ID == input$m_marker_click$id])
        reactive_selection$overall_camera <- input$m_marker_click
        reactive_selection$overall_data_location <- unique(camera_locations$place[camera_locations$camera_ID == input$m_marker_click$id])
        
      
      updateNavbarPage(session,
                       inputId = "nav",
                       selected = "Pictures")
    }
  })
  
  
  local_wl_metadata <- reactive({
    req(input$data_sensor)
    
    input$data_sensor

    sensor_surveys %>% 
      filter(sensor_ID == !!input$data_sensor) %>%
      filter(date_surveyed == max(date_surveyed, na.rm=T)) %>% 
      collect() %>% 
      mutate(types = str_split(wl_types, pattern = ", "),
             url = wl_url)
  })

  alt_local_wl_metadata <- reactive({
    req(input$data_sensor)

    input$data_sensor

    sensor_surveys %>% 
      filter(sensor_ID == !!input$data_sensor) %>%
      filter(date_surveyed == max(date_surveyed, na.rm=T)) %>% 
      collect() %>% 
      mutate(types = str_split(alt_wl_types, pattern = ", "),
             url = alt_wl_url)
  })
  
  output$thirdparty_info <- renderUI({
    wl_metadata_collected <- local_wl_metadata()
    if(!is.na(wl_metadata_collected$wl_url)){
      helpText("  Data source: ",a(href=wl_metadata_collected$wl_url,wl_metadata_collected$wl_src, target = "_blank", class = "pretty-link"))
    }
    
    else(helpText("  Not available"))

  })

  output$alt_thirdparty_info <- renderUI({
    alt_wl_metadata_collected <- alt_local_wl_metadata()
    if(!is.na(alt_wl_metadata_collected$alt_wl_url)){
      helpText("  Data source: ",a(href=alt_wl_metadata_collected$alt_wl_url,alt_wl_metadata_collected$alt_wl_src, target = "_blank", class = "pretty-link"))
    }
  })
  
  
  plot_missing_data_shading <- reactive({
    req(input$data_sensor, isolate(flood_status_reactive()))
    input$refresh_button
    
    if(!isolate(flood_status_reactive())$is_current){
      return(datetime_to_timestamp(sensor_locations %>% filter(sensor_ID == input$data_sensor) %>% pull(date)))
    }
    else(return(NULL))
  })
  
  observeEvent(input$data_sensor, {
    # Always show third party data for CB/FIMAN locations
    if(grepl( 'CB_', input$data_sensor, fixed = TRUE)) {
      updateMaterialSwitch(session, 
                            inputId = "view_3rdparty_data",
                            value = T)
    } else {
      updateMaterialSwitch(session, 
                            inputId = "view_3rdparty_data",
                            value = F)
    }
    shinyjs::show(id = "view_3rdparty_data")
    if(is.na(isolate(local_wl_metadata()$wl_url))){
      shinyjs::disable(id = "view_3rdparty_data")
    }
    
    if(!is.na(isolate(local_wl_metadata()$wl_url))){
      shinyjs::enable(id = "view_3rdparty_data")
    }

    if(is.na(isolate(alt_local_wl_metadata()$alt_wl_url))){
      shinyjs::hide(id = "view_alt_3rdparty_data")
      shinyjs::disable(id = "view_alt_3rdparty_data")
    }
    
    if(!is.na(isolate(alt_local_wl_metadata()$alt_wl_url))){
      shinyjs::show(id = "view_alt_3rdparty_data")
      shinyjs::enable(id = "view_alt_3rdparty_data")
    }
  })
  
  
  observeEvent(c(input$get_plot_data, input$view_3rdparty_data, input$data_sensor),{
    req(input$view_3rdparty_data == T,
        "obs" %in% unlist(isolate(local_wl_metadata())$types),
        abs(input$dateRange[2] - input$dateRange[1]) <=30,
        nrow(sensor_data()!=0))
    
    w$show()
    
    min_date = min(input$dateRange)
    max_date = max(input$dateRange)
    
    plot_sensor_stats <- sensor_locations %>%
      filter(sensor_ID %in% input$data_sensor)
    
    plot_3rd_party_data_obs <<- get_local_wl(wl_id = isolate(local_wl_metadata())$wl_id,
                                             wl_src = isolate(local_wl_metadata())$wl_src,
                                             type = "obs",
                                             begin_date = min_date,
                                             end_date = max_date) %>%
      mutate(date = datetime_to_timestamp(date),
             road_water_level= level-plot_sensor_stats$road_elevation,
             sensor_water_level=level)
    w$hide()
    
  })
  
  observeEvent(c(input$get_plot_data, input$view_3rdparty_data),{
    req(input$view_3rdparty_data == T,
        "pred" %in% unlist(isolate(local_wl_metadata())$types),
        abs(input$dateRange[2] - input$dateRange[1]) <=30,
        nrow(sensor_data()!=0))
    
    min_date = min(input$dateRange)
    max_date = max(input$dateRange)
    
    plot_sensor_stats <- sensor_locations %>% 
      filter(sensor_ID %in% input$data_sensor)
    
    plot_3rd_party_data_predict <<- get_local_wl(wl_id = isolate(local_wl_metadata())$wl_id,
                                                 wl_src = isolate(local_wl_metadata())$wl_src,
                                                 type = "pred",
                                                 begin_date = min_date %>% str_remove_all("-"),
                                                 end_date = max_date %>% str_remove_all("-")) %>% 
      mutate(date = datetime_to_timestamp(date),
             road_water_level= level-plot_sensor_stats$road_elevation,
             sensor_water_level=level)
    
  })

  observeEvent(c(input$get_plot_data, input$view_alt_3rdparty_data, input$data_sensor),{
    req(input$view_alt_3rdparty_data == T,
        "obs" %in% unlist(isolate(alt_local_wl_metadata())$types),
        abs(input$dateRange[2] - input$dateRange[1]) <=30,
        nrow(sensor_data()!=0))

    w$show()

    min_date = min(input$dateRange)
    max_date = max(input$dateRange)

    plot_sensor_stats <- sensor_locations %>%
      filter(sensor_ID %in% input$data_sensor)

    plot_alt_3rd_party_data_obs <<- get_local_wl(wl_id = isolate(alt_local_wl_metadata())$alt_wl_id,
                                             wl_src = isolate(alt_local_wl_metadata())$alt_wl_src,
                                             type = "obs",
                                             begin_date = min_date,
                                             end_date = max_date) %>%
      mutate(date = datetime_to_timestamp(date),
             road_water_level= level-plot_sensor_stats$road_elevation,
             sensor_water_level=level)
    w$hide()
  })
  
  site_info <- reactive({
    req(input$data_sensor)
    return(sensor_surveys %>% 
             filter(sensor_ID == !!input$data_sensor) %>% 
             filter(date_surveyed == max(date_surveyed, na.rm=T)) %>% 
             collect())
  })
  
  # output$construction_banner <- renderUI({
  #   if(site_info()$under_construction == T){
  #     return(
  #       box(title = p("🚧",strong("Site Under Construction"),"🚧"),
  #           width=12,
  #           status = "warning",
  #           solidHeader = T,
  #           collapsed = T,
  #           p("Pardon our mess! We are working to get this sensor site up and running."))
  #     )
  #   }
  # })
  
  output$site_notes <- renderUI({
    switch(site_info()$notes != "NA",
           p(strong("Sensor Installation Date: "),isolate(site_info())$notes),
           "")
    
    # p(strong("Site notes: "),isolate(site_info())$notes)
  })
  
  output$site_description <- renderUI({
    includeMarkdown(paste0("site_descriptions/",isolate(site_info())$sensor_ID,"/",isolate(site_info())$sensor_ID,".md"))
    
  })
  
  # Render plot with selected data
  observe({
    if(nrow(sensor_data()) == 0 & input$view_3rdparty_data == F){
      # Popup on load to display info
      shinyalert::shinyalert(type="error",
                             title="Whoops, no data!",
                             text="We don't have any data during the selected time range. 
                             Please select a different time span or turn on 'Local Water Levels' to view data from other sources.",
                             animation = T)
    }
    
    # Set min and max date using reactive values from date_filter
    min_date_plot <- reactive_min_date() 
    max_date_plot <- reactive_max_date() 
    
    # use spinner for load
    w$show()
    
    # Render the plot
    output$site_level_ts <-  renderHighchart({
      req(nrow(sensor_data()>0))
      
      # Get all plot data
      plot_sensor_data <- sensor_data() 
      
      plot_sensor_stats <- sensor_locations %>% 
        filter(sensor_ID %in% input$data_sensor)

      reference_elevation_info <- reference_elevations %>%
        filter(sensor_ID %in% input$data_sensor) 
      
      x <- plot_sensor_data %>% 
        arrange(date) %>% 
        mutate(date = datetime_to_timestamp(date))
      
      # Plot Road datum
      if(input$elev_datum == "Road") {
        
        road_elevation_limit <- 0
        sensor_elevation_limit <- plot_sensor_stats$sensor_elevation - plot_sensor_stats$road_elevation
        reference_elevation_limit <- reference_elevation_info$reference_elevation - plot_sensor_stats$road_elevation
        y_axis_max <- ifelse(nrow(x) != 0,
                             c(ifelse(max(x$road_water_level_adj, na.rm=T) > road_elevation_limit, max(x$road_water_level_adj, na.rm=T) , road_elevation_limit+0.25)),
                             c(NA))
      }
      
      # Plot NAVD88 Datum
      if(input$elev_datum == "NAVD88") {
        
        road_elevation_limit <- plot_sensor_stats$road_elevation
        sensor_elevation_limit <- plot_sensor_stats$sensor_elevation
        reference_elevation_limit <- reference_elevation_info$reference_elevation
        y_axis_max <- ifelse(nrow(x)!=0,
                             c(ifelse(max(x$sensor_water_level_adj, na.rm=T) > road_elevation_limit, max(x$sensor_water_level_adj, na.rm=T) , road_elevation_limit +0.25 )),
                             c(NA))
      }

      reference_elevation_limit_label <- ifelse(reference_elevation_info$reference_elevation_type == 'drain_bottom', "Bottom of Drain", "Land Elevation")
      if (!is.na(reference_elevation_limit) & input$view_3rdparty_data == F & input$view_alt_3rdparty_data == F) {
        y_axis_min <- ifelse(nrow(x)!=0,
                             c(ifelse(min(x$sensor_water_level_adj, na.rm=T) < reference_elevation_limit, min(x$sensor_water_level_adj, na.rm=T) , reference_elevation_limit - 0.25 )),
                             c(NA)) 
      } else {
         y_axis_min <- NULL
      }
      
      hc <- highchart() %>%
        hc_add_series(data = x %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level_adj","sensor_water_level_adj")),
                      hcaes(x=date,
                            y = wl),
                      type="line",
                      name="Water Level",
                      color="#1d1d75",
                      # Controls when points are shown on plot (only on zoom)
                      marker = list(
                        enabledThreshold = 0.25
                      ),
                      # zones = list(
                      #   list(
                      #   value = sensor_elevation_limit + 0.05,
                      #   color = '#A0A0A0',
                      #   dashStyle = "shortdash"
                      #   ),
                      #   list(
                      #     color = '#1d1d75'
                      #   )
                      # )
                      )%>%
        hc_chart(zoomType= "x",
                 backgroundColor = "#FFF"
        )%>%
        hc_plotOptions(series = list(lineWidth = 2,
                                     # Shows points on line when hovered
                                     allowPointSelect = TRUE,
                                     # Makes line thicker on hover
                                     states = list(hover = list(lineWidth = 2.5)),
                                     # Defines gap size to not connect points with line
                                     gapSize = 2160000,
                                     gapUnit = "value"
                                     
        )) %>%
        hc_tooltip(crosshairs = TRUE,
                   valueDecimals = 2,
                   xDateFormat = "%I:%M %p, %b %e, %Y",
                   shared= TRUE
        ) %>%
        hc_xAxis(type = "datetime",
                 max = max_date_plot %>% datetime_to_timestamp(),
                 min = min_date_plot %>% datetime_to_timestamp(),
                 dateTimeLabelFormats = list(
                   day = "%b %e",
                   minute = "%I:%M %p",
                   hour = "%I:%M %p"
                 ),
                 plotBands = list(
                   list(
                     from = plot_missing_data_shading(),
                     to = datetime_to_timestamp(Sys.time() %>% with_tz("UTC")),
                     color = hex_to_rgba("black", 0.1),
                     label = list(text = "Missing data",
                                  style = list(color = 'grey', fontWeight = 'bold', fontSize = 14),
                                  y = -5),
                     # the zIndex is used to put the label text over the grid lines
                     zIndex = 1
                     
                   )
                 ),
                 plotLines = list(list(value = datetime_to_timestamp(Sys.time() %>% with_tz("UTC")),
                                       # dashStyle = "longdash",
                                       color="black",
                                       width = 1,
                                       zIndex = 4,
                                       label = list(text = "Current time",
                                                    style = list( color = 'black', fontWeight = 'bold'))))) %>%
        hc_yAxis(max = y_axis_max,
                 min = y_axis_min,
                 title = list(text = "Water Level (ft)"),
                 plotLines = list(
                  list(value =reference_elevation_limit,
                        dashStyle = "longdash",
                        color="#ffc300",
                        width = 1,
                        zIndex = 4,
                        label = list(text = reference_elevation_limit_label,
                                     style = list( color = '#ffc300', fontWeight = 'bold'))),
                   list(value =road_elevation_limit,
                        dashStyle = "longdash",
                        color="red",
                        width = 1,
                        zIndex = 4,
                        label = list(text = "Road Elevation",
                                     style = list( color = 'red', fontWeight = 'bold'))),
                   list(value = sensor_elevation_limit,
                        dashStyle = "longdash",
                        color="black",
                        width = 1,#6
                        zIndex = 1,
                        label = list(text = "Sensor Elevation*",
                                     style = list( color = 'black', fontWeight = 'bold'))))) %>%
        hc_exporting(enabled = TRUE,
                     filename = paste0(plot_sensor_stats$sensor_ID,"_",min_date_plot %>% with_tz("America/New_York"),"_to_",max_date_plot %>% with_tz("America/New_York")),
                     showTable = F,
                     buttons = list(contextButton = list(symbolSize = 20,
                                                         x= -20,
                                                         menuItems = list("viewFullscreen", "printChart", "separator", "downloadPNG")))) %>%
        hc_title(text =plot_sensor_stats$sensor_label,
                 floating = F)
      
      if(input$view_3rdparty_data == T){
        
        plot_3rd_party_data_stats <- local_wl_metadata()
        
        if("obs" %in% unlist(plot_3rd_party_data_stats$types)){
          hc <- hc %>% hc_add_series(plot_3rd_party_data_obs %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level","sensor_water_level")),
                                     hcaes(x=date,
                                           y=wl),
                                     name = unique(plot_3rd_party_data_obs$entity),
                                     type="line",
                                     color="#DC018A",
                                     visible = T)
        }
        
        if("pred" %in% unlist(plot_3rd_party_data_stats$types)){
          hc <- hc %>% hc_add_series(plot_3rd_party_data_predict %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level","sensor_water_level")),
                                     hcaes(x=date,
                                           y=wl),
                                     name = unique(plot_3rd_party_data_predict$entity),
                                     type="line",
                                     color="#01CB4D",
                                     visible = T)
        }
      }

      if(input$view_alt_3rdparty_data == T) {

        plot_alt_3rd_party_data_stats <- alt_local_wl_metadata()

        if("obs" %in% unlist(plot_alt_3rd_party_data_stats$types)) {
          hc <- hc %>% hc_add_series(plot_alt_3rd_party_data_obs %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level","sensor_water_level")),
                                     hcaes(x=date,
                                           y=wl),
                                     name = unique(plot_alt_3rd_party_data_obs$entity),
                                     type="line",
                                     color="#01DC15",
                                     visible = T)
        }
      }
      
      if(admin_login_status() == T){
        hc <- hc %>% 
          hc_add_series(data = x %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level","sensor_water_level")),
                        hcaes(x=date,
                              y = wl),
                        type="line",
                        name="Water Level (unadjusted)",
                        color="#4da9f0",
                        visible = T,
                        # Controls when points are shown on plot (only on zoom)
                        marker = list(
                          enabledThreshold = 0.25
                        )) %>% 
          hc_add_series(data = x %>% dplyr::select(date,voltage),
                        hcaes(x=date,
                              y = voltage/10),
                        type="line",
                        name="Battery (Voltage/10)",
                        color="#ffde05",
                        visible = F,
                        # Controls when points are shown on plot (only on zoom)
                        marker = list(
                          enabledThreshold = 0.25
                        ))
      }
      
      hc
      
    })
    
  })
  
  observe({
    req(input$camera_ID)

    w2$show()
    
    output$camera <- renderImage({
      
      outfile <- tempfile(fileext='.jpg')
      
      time <- con %>% 
        tbl("photo_info") %>% 
        group_by(camera_ID) %>% 
        filter(DateTimeOriginalUTC == max(DateTimeOriginalUTC, na.rm=T)) %>% 
        collect()
      
      realtime_img <- magick::image_read(paste0("https://photos-sunnydayflood.apps.cloudapps.unc.edu/public/",input$camera_ID,".jpg")) 
      
      site_time <- time %>% 
        filter(camera_ID == input$camera_ID) %>% 
        pull(DateTimeOriginalUTC) %>%
        lubridate::with_tz(tzone="America/New_York")
    
      realtime_img %>% 
        image_annotate(paste0(format(site_time, "%I:%M %p", usetz = T)," - ",format(site_time, "%b %d, %Y")), size = 40, color = "white", boxcolor = "black",
                       degrees = 0, gravity = "north") %>%
        magick::image_write(path = outfile)
      
      # Return a list
      list(src = outfile,
           alt = paste0("Latest picture from",input$camera_ID))
      
    }, deleteFile = T)
    
    # output$camml <- renderUI({
    #   req <- httr::POST("https://ml-acgold.apps.cloudapps.unc.edu/detect_flooding_latest",
    #                     body = list(camera_ID=input$camera_ID),
    #                     encode = "json")
    # })
    
    #             output$tf_predict <- renderHighchart({
    
    # req <- httr::POST("https://ml-sunnydayflood.apps.cloudapps.unc.edu/detect_flooding_latest",
    #                   body = list(camera_ID=input$camera_ID),
    #                   encode = "json")
    
    #               prediction <- round(unlist(httr::content(req)),digits = 2)
    
    
    #               col_stops <- data.frame(
    #                 q = c(0.4, 0.6, .8),
    #                 c = c('#48bf84','#f5bf0f','#e1142c'),
    #                 stringsAsFactors = FALSE
    #               )
    
    #               highchart() %>%
    #                 hc_chart(type = "solidgauge") %>%
    #                 hc_pane(
    #                   startAngle = -90,
    #                   endAngle = 90,
    #                   background = list(
    #                     outerRadius = '100%',
    #                     innerRadius = '60%',
    #                     shape = "arc"
    #                   )
    #                 ) %>%
    #                 hc_tooltip(enabled = FALSE) %>%
    #                 hc_yAxis(
    #                   stops = list_parse2(col_stops),
    #                   lineWidth = 0,
    #                   minorTickWidth = 0,
    #                   tickAmount = 2,
    #                   min = 0,
    #                   max = 100,
    #                   labels = list(y = 26, style = list(fontSize = "22px"))
    #                 ) %>%
    #                 hc_add_series(
    #                   data = prediction*100,
    #                   dataLabels = list(
    #                     y = -50,
    #                     borderWidth = 0,
    #                     format= "{y} %",
    #                     useHTML = TRUE,
    #                     style = list(fontSize = "40px")
    #                   )
    #                 ) %>%
    #                 hc_size(height = 300)
    
    #             })
    w2$hide()
  })

  observe({
    req(input$data_sensor)

    w3$show()

    if(grepl( "DE", input$data_sensor, fixed = TRUE)) {
          image_src = "images/SunnyD_MarshSchematic.png"
          guide_text = "This water level sensor is installed on the side of a road just above ground level. Measurements from our sensor (the blue line on the data plot) above the dashed red line indicate flooding on the road."
    } else {
          image_src = "images/SunnyD_RoadwaySchematic.png"
          guide_text = "This water level sensor is installed in a storm drain. Measurements from our sensor (the blue line) above the red dotted line indicate flooding on the road."
    }

    output$guide_text <- renderText({ guide_text })

    output$guide <- renderImage({
        
        outfile <- tempfile(fileext='.png')
        realtime_img <- magick::image_read(image_src) 
        realtime_img %>% 
          magick::image_write(path = outfile)
        
        # Return a list
        list(src = outfile,
            alt = "Schematic showing how to read plot",
            height = "100%")
        
      }, deleteFile = T)

    w3$hide()
  })
  
  
#---------  Sensor dashboard panels -----------
  output$dashboard_panels <- renderUI({
    sensors <- sensor_locations %>% 
      left_join(isolate(map_flood_status_reactive()), by = "sensor_ID") %>%
      arrange(sensor_ID) %>% 
      left_join(con %>% 
                  tbl("sensor_data") %>% 
                  group_by(sensor_ID) %>% 
                  slice_max(date, n=1) %>% 
                  collect() %>% 
                  dplyr::select(place, sensor_ID, voltage, processed, raw_data_date = date)
                )
    
    cameras <- camera_locations %>% 
      arrange(camera_ID) %>% 
      mutate(
        time_since_measurement = as.numeric(floor(difftime(Sys.time(),date_lst, unit = "mins"))),
        time_since_measurement_text = purrr::map(.x = time_since_measurement, .f = time_converter)
      )
    

    places <- c(sensors$place, cameras$place) %>% unique()
    
    n_places <- length(places)
    
    n_sensors <- nrow(sensors)
    
    n_cameras <- nrow(cameras)
    
    all_panels <- foreach(j = 1:n_places) %do% {
      filtered_sensors <- sensors %>% 
        filter(place == places[j])
      
      filtered_cameras <- cameras %>% 
        filter(place == places[j])
      
      n_filtered_sensors <- nrow(filtered_sensors)
      
      n_filtered_cameras <- nrow(filtered_cameras)

      if (n_filtered_sensors == 0) {
        sensor_panels <- ""
      } else {
        sensor_panels <- foreach(i = 1:n_filtered_sensors) %do% {
          if(filtered_sensors$flood_status[i] == "NOT FLOODING"){
            sensor_label <- boxLabel(text = "Good!", status = "success")
          }
          if(filtered_sensors$flood_status[i] != "NOT FLOODING"){
            sensor_label <- boxLabel(text = "Bad!", status = "danger")
          }
          
          matched_camera <- filtered_cameras %>%
            filter(str_remove(camera_ID, "CAM_") == filtered_sensors$sensor_ID[i])

          no_data_error <- ifelse(filtered_sensors$flood_status[i] != "NOT FLOODING" & filtered_sensors$raw_data_date[i] == filtered_sensors$date[i] & matched_camera$time_since_measurement[i] < 10, T, F)
          gateway_error <- ifelse(filtered_sensors$flood_status[i] != "NOT FLOODING" & filtered_sensors$raw_data_date[i] == filtered_sensors$date[i] & matched_camera$time_since_measurement[i] > 10, T, F)
          processing_data_error <- ifelse(filtered_sensors$flood_status[i] != "NOT FLOODING" & filtered_sensors$processed[i] == F, T, F)

          no_data_error_icon <- ifelse(no_data_error,
                                      as.character(icon("times-circle", style="color:#dc3545 !important")),
                                      as.character(icon("check-circle",style="color:#28a745 !important")))

          gateway_error_icon <- ifelse(gateway_error,
                                      as.character(icon("times-circle", style="color:#dc3545 !important")),
                                      as.character(icon("check-circle",style="color:#28a745 !important")))

          processing_data_error_icon <- ifelse(processing_data_error,
                                        as.character(icon("times-circle", style="color:#dc3545 !important")),
                                        as.character(icon("check-circle",style="color:#28a745 !important")))

          error_table <- tibble("Error Status" = c("Sensor", "Gateway", "Processing"),
                                "Icons" = c(no_data_error_icon, gateway_error_icon, processing_data_error_icon),
                                "values" = c(no_data_error, gateway_error, processing_data_error))
          
          error_table_as_html <- HTML(paste0('
                                      <table class = "center">
                                        <tr>
                                          <th>  </th>
                                          <th> Status </th>
                                        </tr>
                                        <tr>
                                          <td> Sensor </td>
                                          <td>', error_table[1,2],'</td>
                                        </tr>
                                        <tr>
                                          <td> Gateway </td>
                                          <td>', error_table[2,2],'</td>
                                        </tr>
                                        <tr>
                                          <td> Processing </td>
                                          <td>', error_table[3,2],'</td>
                                        </tr>
                                      </table>'
                                        )
          )
          
          col_stops <- data.frame(
            q = c(3.6, 3.4, 3.3),
            c = c('#55BF3B', '#DDDF0D', '#DF5353'),
            stringsAsFactors = FALSE
          )

          
          box(width = 12,
              title = filtered_sensors$sensor_ID[i],
              label = sensor_label,            
              status = "gray-dark",
              solidHeader = T,
              elevation = 1,
              div(class = "col-sm-12",
                  fluidRow(p(strong("Details"), style="font-size:20px")),
                  br(),
                  fluidRow(p("Last measurement: ", HTML(filtered_sensors$time_since_measurement_text[i]))),
                  fluidRow(p("Status: ", strong(filtered_sensors$flood_status[i])))
                ),
              hr(),
              div(class = "col-sm-12",
                  fluidRow(p(strong("Status Table"), style="font-size:20px")),
                  fluidRow(error_table_as_html)
                ),
              hr(),
              div(class = "col-sm-12",
                  fluidRow(p(strong("Battery Voltage"), style="font-size:20px")),
              
              highchart() %>%
                hc_chart(type = "solidgauge") %>%
                hc_pane(
                  startAngle = -90,
                  endAngle = 90,
                  background = list(
                    outerRadius = '100%',
                    innerRadius = '60%',
                    shape = "arc"
                  )
                ) %>%
                hc_tooltip(enabled = T) %>% 
                hc_yAxis(
                  stops = list_parse2(col_stops),
                  lineWidth = 0,
                  minorTickWidth = .25,
                  tickAmount = .25,
                  min = 3.2,
                  max = 5.4,
                  labels = list(y = 26, style = list(fontSize = "16px"))
                ) %>%
                hc_add_series(
                  data = filtered_sensors$voltage[i],
                  dataLabels = list(
                    y = 50,
                    borderWidth = 0,
                    useHTML = TRUE,
                    style = list(fontSize = "30px")
                  )
                ) %>% 
                hc_size(height = 300)
              )
                      
              
          )
        }
      }
      
      camera_panels <- foreach(i = 1:n_filtered_cameras) %do% {
        if(filtered_cameras$time_since_measurement[i] < 10){
          camera_label <- boxLabel(text = "Good!", status = "success")
        } 
        if(filtered_cameras$time_since_measurement[i] >= 10){
          camera_label <- boxLabel(text = "Bad!", status = "danger")
        }
                               
        
        box(width = 12,
            title = filtered_cameras$camera_ID[i],
            status = "gray-dark",
            solidHeader = T,
            elevation = 1,
            collapsible = F,
            label = camera_label,
            fluidRow(p("Last picture: ", HTML(unlist(filtered_cameras$time_since_measurement_text[i]))))
        )
      }
      
      box(title = places[j],
          status = "secondary",
          boxToolSize = "md",
          solidHeader = T,
          width=12,
          fluidRow(box(title=strong("Sensors"),
              width = 6,
              fluidRow(sensor_panels),
              headerBorder = F,
              elevation = 0
          ),
          box(title=strong("Cameras"),
              width = 6,
              fluidRow(camera_panels),
              headerBorder = F,
              elevation = 0
          )
          )
      )
      
    }
    
    all_panels
    
  })

  updateSelectFromURL <- function(query) {
    updateSelectInput(session, "city_name", selected=query[['location']])
  }

  zoomToSensorFromURL <- function(query) {
    selected <- query[['sensor_ID']]
    selected_sensor <- con %>%
      tbl('sensor_surveys') %>% 
      filter(sensor_ID == selected) %>%
      slice_max(date_surveyed, n=1) %>%
      collect()

    if (nrow(selected_sensor) == 1) {
      leafletProxy(mapId = "m") %>% 
        setView(lng = selected_sensor$lng, lat = selected_sensor$lat, zoom=20)
    }
  }

  observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['sensor_ID']])) {
        delay(1000, zoomToSensorFromURL(query))
      } else if (!is.null(query[['location']])) {
        delay(1000, updateSelectFromURL(query))
      }  
  })
  
  waiter::waiter_hide()
}

# Run the application
shinyApp(ui = ui, server = server)
