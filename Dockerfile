# Base image https://hub.docker.com/u/rocker/
FROM dukegcb/openshift-shiny-verse:4.0.2

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    # libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libudunits2-dev 

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## app folder
COPY /sunnyd-shinyapp ./app

## renv.lock file
## COPY /sunnyd-shinyapp/renv.lock ./renv.lock

# install renv & restore packages
RUN install2.r tidyverse shiny shinyjs rsconnect shinythemes shinydashboard colourvalues waiter sf leaflet raster rgdal lwgeom ggrepel DT htmltools RColorBrewer lubridate plotly units
# RUN R -e "install.packages(c('renv', 'tidyverse','shiny','shinyjs','rsconnect', 'shinythemes','shinydashboard','colourvalues','waiter', 'sf',  'leaflet', 'raster', 'rgdal', 'lwgeom', 'ggrepel', 'DT', 'htmltools', 'RColorBrewer', 'lubridate', 'plotly', 'units'), repos='http://cran.rstudio.com/', Ncpus = 6)"

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
