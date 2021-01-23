# Base image https://hub.docker.com/u/rocker/
# FROM dukegcb/openshift-shiny-verse:4.0.2
FROM rocker/shiny-verse:latest

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chown -R shiny /var/lib/shiny-server/

# OpenShift gives a random uid for the user and some programs try to find a username from the /etc/passwd.
# Let user to fix it, but obviously this shouldn't be run outside OpenShift
RUN chmod ug+rw /etc/passwd
COPY fix-username.sh /usr/bin/fix-username.sh
COPY shiny-server.sh /usr/bin/shiny-server.sh

## app folder
#COPY /sunnyd-shinyapp ./app
ADD ./sunnyd-shinyapp /srv/code

# install packages
RUN install2.r tidyverse shiny shinyjs rsconnect shinythemes shinydashboard colourvalues waiter sf leaflet raster rgdal lwgeom ggrepel DT htmltools RColorBrewer lubridate plotly units

RUN chmod a+rx /usr/bin/shiny-server.sh

# Make sure the directory for individual app logs exists and is usable
RUN chmod -R a+rwX /var/log/shiny-server
RUN chmod -R a+rwX /var/lib/shiny-server

CMD /usr/bin/shiny-server.sh

# expose port
# EXPOSE 3838

# run app on container start
# CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
