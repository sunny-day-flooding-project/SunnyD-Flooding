# Base image https://hub.docker.com/u/rocker/
# FROM rocker/shiny-verse:latest
FROM dukegcb/openshift-shiny-verse:4.0.2

# # system libraries of general use
# ## install debian packages
# RUN apt-get update -qq && apt-get -y --no-install-recommends install \
#     libxml2-dev \
#     libcairo2-dev \
#     libsqlite3-dev \
#     #libmariadbd-dev \
#     libpq-dev \
#     libssh2-1-dev \
#     unixodbc-dev \
#     libcurl4-openssl-dev \
#     libssl-dev \
#     libudunits2-dev \
#     libgdal-dev

# ## update system libraries
# RUN apt-get update && \
#     apt-get upgrade -y && \
#     apt-get clean

RUN install2.r shinythemes shinydashboard colourvalues waiter sf leaflet raster rgdal lwgeom DT htmltools RColorBrewer lubridate plotly units

# copy necessary files
## app folder
# COPY /sunnyd-shinyapp ./app
COPY /sunnyd-shinyapp /srv/code
# # install renv & restore packages

# # expose port
# EXPOSE 3838

# # run app on container start
# CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
