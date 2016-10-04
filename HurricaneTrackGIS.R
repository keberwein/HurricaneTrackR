# Forked from https://rud.is/b/2015/08/20/track-hurricane-danny-with-r-leaflet/
# Requires devtools::install_github('rstudio/leaflet')

library(xml2)
library(plyr)
library(leaflet)
library(stringi)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(rvest)

## Get GIS data
load("NOAA_GIS.Rdata")

# Storm scale
ss <-  c("Tropical Depression", "Tropical Storm", "Hurricane-1", "Hurricane-2", "Major Hurricane-3", "Major Hurricane-4", "Major Hurricane-5")
pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))

storm <- storm[order(storm$TAU),]

storm$status <- paste(storm$TCDVLP, storm$SSNUM, sep='-')

storm$color <- as.character(factor(storm$status, levels = ss, labels = pal))

storm$advisory <- as.POSIXct(storm$ADVDATE, format='%y%m%d/%H%M')

m <- # Create leaflet map.
    leaflet(data=storm, width=1024, height=768) %>%
    addTiles() %>%
    addWMSTiles(
        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        layers = "nexrad-n0r-900913",
        options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>%
    addGeoJSON(shp, stroke = TRUE, color = 'grey', fill = FALSE) %>%
    addGeoJSON(lin) %>%
    addCircleMarkers(~LON, ~LAT, radius = ~SSNUM * 4, stroke = TRUE, color = ~color,
                     opacity = 1, weight = 2, fill = TRUE, fillColor = ~color,
                     popup = ~sprintf("<b>Advisory forecast %s: %s</b><hr noshade size='1'/>
                                      Time: %s %s<br/>
                                      Position: %3.2f, %3.2f<br/>
                                      Status: <strong>%s</strong><br/>
                                      Wind: %s<br/>Gust: %s (knots)",
                                      htmlEscape(ADVISNUM), htmlEscape(format(advisory, "%b %d %H:%M")),
                                      htmlEscape(DATELBL), htmlEscape(TIMEZONE),
                                      htmlEscape(LON), htmlEscape(LAT),
                                      htmlEscape(status),
                                      htmlEscape(MAXWIND), htmlEscape(GUST))

    ) %>%
    addLegend("bottomright", colors = pal, labels = ss)

html_print(m)

saveWidget(m, '/tmp/trackr.html', selfcontained = TRUE)
