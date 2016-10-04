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

#html <- read_html('http://weather.unisys.com/hurricane/atlantic/2016/index.php')
#links <- html_attr(html_nodes(html, "a"), "href")
#links <- links[grep('track.dat', links)]

# get track data
#track <- select.list(links, title="Select storm:", graphics = FALSE)
#url <- paste("http://weather.unisys.com/hurricane/atlantic/2016", track, sep="/")

url <- "http://weather.unisys.com/hurricane/atlantic/2016/MATTHEW/track.dat"

storm <- readLines(url)
storm <- read.table(textConnection(gsub("TROPICAL ", "TROPICAL_", storm[3:length(storm)])), header=TRUE, stringsAsFactors=FALSE)

# make storm type names prettier
storm$STAT <- stri_trans_totitle(gsub("_", " ", storm$STAT))

# make column names prettier
colnames(storm) <- c("advisory", "lat", "lon", "time", "wind_speed", "pressure", "status")

# Storm scale
ss <-  c("Tropical Depression", "Tropical Storm", "Hurricane-1", "Hurricane-2", "Hurricane-3", "Hurricane-4", "Hurricane-5")
pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))

storm$color <- as.character(factor(storm$status, levels = ss, labels = pal))

# lighten past position colors
storm$opacity <- 0.5
storm$opacity[strptime(storm$time, format="%m/%d/%H") <= Sys.time()] <- 0.1

# make windspeeds useful for point sizes
storm$wind_speed[storm$wind_speed == "-"] <- 0
storm$wind_speed <- as.integer(storm$wind_speed)
storm$wind_speed[is.na(storm$wind_speed)] <- 0

storm$time <- gsub("Z", "", storm$time)
storm$date <- strftime(strptime(storm$time, format="%m/%d/%H"), '%m/%d %Hh')

# separate complete and intermediate advisories (assuming they come in pairs - TODO)
storm$adv <- gsub("A", "", storm$advisory)
storm <- ddply(storm, "adv", head, 1)
storm <- storm[order(storm$date),]

storm$time <- as.POSIXct(storm$time, format='%m/%d/%H', tz="UTC")
storm$localtime <- as.POSIXct(format(storm$time, tz=Sys.timezone(), usetz = TRUE))
storm$day <- paste(weekdays(storm$localtime, abbreviate = TRUE), format(storm$localtime, "%H:%M"), " ")

shp <- file_to_geojson("/Users/mkeranen/Downloads/al142016_5day_latest/al142016-023_5day_pgn.shp", method='local', output=':memory:')

m <- # Create leaflet map.
    leaflet(data=storm, width=1024, height=768) %>%
    addTiles() %>%
    addWMSTiles(
        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        layers = "nexrad-n0r-900913",
        options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>%
    addPolylines(~lon, ~lat, color = 'grey', weight=3) %>%
    addCircleMarkers(~lon, ~lat, radius = ~wind_speed / 3, stroke = TRUE, color = 'grey',
                     opacity = 1, weight = 2, fill = true, fillColor = ~color,
                     fillOpacity = ~opacity,
                     popup = ~sprintf("<b>Advisory forecast %s (%s)</b><hr noshade size='1'/>
                                      Local time: %s<br/>
                                      Position: %3.2f, %3.2f<br/>
                                      Strength: <strong>%s</strong><br/>
                                      Wind: %s (knots)<br/>Pressure: %s",
                                      htmlEscape(advisory), htmlEscape(date), htmlEscape(format(localtime, "%b %d %H:%M")),
                                      htmlEscape(lon), htmlEscape(lat),
                                      htmlEscape(status), htmlEscape(wind_speed), htmlEscape(pressure))
    ) %>%
    addLegend("bottomright", colors = pal, labels = ss)

    html_print(m)

    #saveWidget(m, '/tmp/trackr.html', selfcontained = FALSE)
