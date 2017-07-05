library(leaflet)
library(stringi)
library(htmltools)
library(RColorBrewer)

danny <- readLines("http://weather.unisys.com/hurricane/atlantic/2016/MATTHEW/track.dat")

danny_dat <- read.table(textConnection(gsub("TROPICAL ", "TROPICAL_", danny[3:length(danny)])), 
                        header=TRUE, stringsAsFactors=FALSE)

# make storm type names prettier
danny_dat$STAT <- stri_trans_totitle(gsub("_", " ", danny_dat$STAT))

# make column names prettier
colnames(danny_dat) <- c("advisory", "lat", "lon", "time", "wind_speed", "pressure", "status")

danny_dat$color <- as.character(factor(danny_dat$status, 
                                       levels=c("Tropical Depression", "Tropical Storm",
                                                "Hurricane-1", "Hurricane-2", "Hurricane-3",
                                                "Hurricane-4", "Hurricane-5"),
                                       labels=rev(brewer.pal(7, "YlOrBr"))))

last_advisory <- tail(which(grepl("^[[:digit:]]+$", danny_dat$advisory)), 1)

# draw the map
leaflet() %>% 
    addTiles(options = tileOptions(detectRetina = TRUE)) %>% 
    
    addPolylines(data=danny_dat[1:last_advisory,], ~lon, ~lat, color=~color) -> tmp_map

if (last_advisory < nrow(danny_dat)) {
    
    tmp_map <- tmp_map %>% 
        addCircles(data=danny_dat[6:nrow(danny_dat),], ~lon, ~lat, color=~color, fill=~color, radius=25000,
                   popup=~sprintf("<b>Advisory forecast for +%sh (%s)</b><hr noshade size='1'/>
                                  Position: %3.2f, %3.2f<br/>
                                  Expected strength: <span style='color:%s'><strong>%s</strong></span><br/>
                                  Forecast wind: %s (knots)<br/>Forecast pressure: %s",
                                  htmlEscape(advisory), htmlEscape(time), htmlEscape(lon),
                                  htmlEscape(lat), htmlEscape(color), htmlEscape(status), 
                                  htmlEscape(wind_speed), htmlEscape(pressure)))
}

html_print(tmp_map)


d <- dir(td, "*_5day_pts.dbf$")
l <- dir(td, "*_5day_lin.shp$")
p <- dir(td, "*_5day_pts.shp$")
s <- dir(td, "*_5day_pgn.shp$")
w <- dir(td, "*_wwlin.shp$")

m <- # create leaflet map
    leaflet(data=danny_dat) %>%
    addTiles(options = tileOptions(detectRetina = TRUE)) %>%
    addWMSTiles(
        "ç",
        layers = "1",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "Weather data: nowcoast.noaa.gov"
    ) %>%
    addCircles(data=danny_dat[6:nrow(danny_dat),], ~lon, ~lat, color=~color, fill=~color, radius=25000,
               popup=~sprintf("<b>Advisory forecast for +%sh (%s)</b><hr noshade size='1'/>
                              Position: %3.2f, %3.2f<br/>
                              Expected strength: <span style='color:%s'><strong>%s</strong></span><br/>
                              Forecast wind: %s (knots)<br/>Forecast pressure: %s",
                              htmlEscape(advisory), htmlEscape(time), htmlEscape(lon),
                              htmlEscape(lat), htmlEscape(color), htmlEscape(status), 
                              htmlEscape(wind_speed), htmlEscape(pressure))) %>%
    addGeoJSON(shp, stroke = TRUE, color = 'grey', fill = FALSE) %>%
    addGeoJSON(lin, weight = 2, fill = FALSE) 


