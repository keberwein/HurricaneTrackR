# Check for required packages, install them if not installed
pkgs <-c('XML', 'plyr', 'leaflet', 'htmltools', 'htmlwidgets', 'RColorBrewer', 'rvest', 'foreign', 'geojsonio')
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) { install.packages(p) }
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm(p, pkgs)

stormname = "MATTHEW"

stormname <- as.character(toupper(stormname))

# get GIS shapefile data and create Rdata bundle
getStorm <- function(stormname) {
    wd <- getwd()
    td <- tempdir()
    setwd(td)

    message("Getting file links")
    gis_at <- read_xml("http://www.nhc.noaa.gov/gis-at.xml")
    gis_doc <- xmlParse(gis_at)
    links <<- xmlToDataFrame(gis_doc, nodes=getNodeSet(gis_doc, "//item"))

    # get advisory shapefile links
    message("Getting NOAA GIS data")
    adv <- links[grep("Advisory [#0-9A-Z]+ Forecast \\[shp\\]", links$title),]
    adv <- adv[grep(stormname, adv$title),]

    # get advisory number
    advnum <<- regmatches(adv$title, regexpr('#[0-9]+[A-Z]?', adv$title))
    advnum <<- sub("#0?", "", advnum)
    message(paste("Current advisory", advnum, sep = " "))

    # get url for storm files
    surl <- adv$link[grep(stormname, adv$title)]

    temp <- tempfile(fileext = ".zip")
    download.file(surl, temp)
    unzip(temp)
    unlink(temp)

    d <- dir(td, "*_5day_pts.dbf$")
    l <- dir(td, "*_5day_lin.shp$")
    p <- dir(td, "*_5day_pts.shp$")
    s <- dir(td, "*_5day_pgn.shp$")
    w <- dir(td, "*_wwlin.shp$")

    storm <<- read.dbf(d)
    lin <<- file_to_geojson(l, method='local', output=':memory:')
    pts <<- file_to_geojson(p, method='local', output=':memory:')
    shp <<- file_to_geojson(s, method='local', output=':memory:')
    # check for watches and warning data
    if (length(w) > 0) {
        message("Watches/Warnings present")
        ww  <<- file_to_geojson(w, method='local', output=':memory:')
    }

    # get wind shapefile links
    wnd <- links[grep("Advisory [#0-9A-Z]+ Wind Field \\[shp\\]", links$title),]
    wurl <- wnd$link[grep(stormname, wnd$title)]

    # not all advisories have wind radius data
    if (length(wurl) > 0) {
        message("Getting NOAA wind data")

        temp <- tempfile(fileext = ".zip")
        download.file(wurl, temp)
        unzip(temp)
        unlink(temp)

        d <- dir(td, "*_forecastradii.dbf$")
        s <- dir(td, "*_forecastradii.shp$")

        wind <<- read.dbf(d)
        radii <<- file_to_geojson(s, method='local', output=':memory:')

    } else {
        message("No link for wind radii")
    }

    unlink(dir(td))
    setwd(wd)
}

# storm scale
ss <-  c("Tropical Depression", "Tropical Storm-0", "Hurricane-1", "Hurricane-2", "Major Hurricane-3", "Major Hurricane-4", "Major Hurricane-5")
pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))

getStorm(stormname)

storm <- storm[order(storm$TAU),]

storm$status <- paste(storm$TCDVLP, storm$SSNUM, sep='-')
storm$color <- as.character(factor(storm$status, levels = ss, labels = pal))
storm$advisory <- as.POSIXct(storm$ADVDATE, format='%y%m%d/%H%M')

title = paste("Storm", stormname, sep = " ")
atime = paste("Adv", storm$ADVISNUM[1], as.character(format(storm$advisory[1], "%b %d %H:%M")), "GMT", sep = " ")
if (!exists("radii")) { atime = paste(atime, "(no winds)", sep = " ")}
rtime = paste("NEXRAD", format(Sys.time(), "%r"), sep = " ")

m <- # create leaflet map
    leaflet(data=storm) %>%
    addTiles(options = tileOptions(detectRetina = TRUE)) %>%
    addWMSTiles(
        "ç",
        layers = "1",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "Weather data: nowcoast.noaa.gov"
    ) %>%
    addGeoJSON(shp, stroke = TRUE, color = 'grey', fill = FALSE) %>%
    addGeoJSON(lin, weight = 2, fill = FALSE) %>%
    addLegend("bottomright", colors = pal, labels = ss, title = title) %>%
    addLegend("topright", colors = NULL, labels = NULL, title = atime) %>%
    addLegend("topright", colors = NULL, labels = NULL, title = rtime)

# add wind radii if available in advisory
if (exists("radii")) {
    m <- addGeoJSON(m, radii, color = 'grey', opacity = 1, stroke = TRUE, weight = 1)
}

if (exists("ww")) {
    m <- addGeoJSON(m, ww, color = 'red', fill = FALSE)
}

m <- addCircles(m, lng = ~LON, lat = ~LAT, radius = ~MAXWIND * 250, color = ~color,
           opacity = 1, weight = 2, fill = TRUE, fillColor = ~color,
           popup = ~sprintf("Time: %s %s<br/>
                            Status: <strong>%s</strong><br/>
                            Position: %3.2f, %3.2f<br/>
                            Wind: %s kts<br/>
                            Gust: %s kts",
                            htmlEscape(DATELBL), htmlEscape(TIMEZONE),
                            htmlEscape(status),
                            htmlEscape(LON), htmlEscape(LAT),
                            htmlEscape(MAXWIND),
                            htmlEscape(GUST))

)

if (interactive()) {
    html_print(m)
} else {
    saveWidget(m, '/var/www/html/trackr.html', selfcontained = FALSE)
}
