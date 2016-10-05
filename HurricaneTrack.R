# Forked from https://rud.is/b/2015/08/20/track-hurricane-danny-with-r-leaflet/
# ? Requires devtools::install_github('rstudio/leaflet')

# Check for required packages, install them if not installed
pkgs <-c('XML', 'plyr', 'leaflet', 'htmltools', 'htmlwidgets', 'RColorBrewer', 'rvest', 'foreign', 'geojsonio')
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) { install.packages(p) }
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm(p, pkgs)

stormname = "MATTHEW"

getCurrentAdv <- function(stormname) {
    gis_at <- read_xml("http://www.nhc.noaa.gov/gis-at.xml")
    gis_doc <- xmlParse(gis_at)
    links <- xmlToDataFrame(gis_doc, nodes=getNodeSet(gis_doc, "//item"))

    # keep only Advisory shapefile links
    links <- links[grep(paste("Advisory [#0-9A-Z]+ Forecast \\[shp\\] - [a-z A-Z]+", stormname, sep = " "), links$title),]

    adv <- regmatches(links$title, regexpr('#[0-9]+[A-Z]?', links$title))
    adv <- sub("#0?", "", adv)

    print(paste("Current advisory", adv, sep = " "))

    return(adv)
}

getStorm <- function(stormname) {
    print("Getting NOAA GIS data")
    wd <- getwd()
    td <- tempdir()
    setwd(td)

    gis_at <- read_xml("http://www.nhc.noaa.gov/gis-at.xml")
    gis_doc <- xmlParse(gis_at)
    links <<- xmlToDataFrame(gis_doc, nodes=getNodeSet(gis_doc, "//item"))

    # get advisory shapefile links
    adv <- links[grep("Advisory [#0-9A-Z]+ Forecast \\[shp\\]", links$title),]
    # cleanup titles for menu
    adv$title <- gsub('\\[shp\\] ', "", adv$title)
    # add advisory number
    adv$advisory <- regmatches(adv$title, regexpr('#[0-9]+[A-Z]?', adv$title))

    ## interactive storm selection
    #l <- select.list(adv$title, title="Select storm:", graphics = FALSE)
    #adv$link[adv$title == l]

    url <- adv$link[grep(stormname, adv$title)]

    temp <- tempfile(fileext = ".zip")
    download.file(url, temp)
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
    ww  <<- file_to_geojson(w, method='local', output=':memory:')

    rm(d, l, p, s, w, gis_at, gis_doc, adv)

    # get wind shapefile links
    wnd <- links[grep("Advisory [#0-9A-Z]+ Wind Field \\[shp\\]", links$title),]
    url <- wnd$link[grep(stormname, wnd$title)]

    temp <- tempfile(fileext = ".zip")
    download.file(url, temp)
    unzip(temp)
    unlink(temp)

    d <- dir(td, "*_forecastradii.dbf$")
    s <- dir(td, "*_forecastradii.shp$")

    wind <<- read.dbf(d)
    radii <<- file_to_geojson(s, method='local', output=':memory:')

    rm(d, s, wnd)

    unlink(dir(td))
    setwd(wd)

    save.image("/tmp/NOAA_GIS.Rdata")
}


# load local GIS data to save time pulling infrequent GIS updates, but keep NEXRAD data current
advnum <- getCurrentAdv(stormname)
if (!file.exists("/tmp/NOAA_GIS.Rdata")) {
    getStorm(stormname)
} else {
    load("/tmp/NOAA_GIS.Rdata")
}

# repull GIS data if not current
if (advnum != storm$ADVISNUM[1]) {
    getStorm(stormname)
} else {
    print("Using exsiting GIS data")
}

# storm scale
ss <-  c("Tropical Depression", "Tropical Storm", "Hurricane-1", "Hurricane-2", "Major Hurricane-3", "Major Hurricane-4", "Major Hurricane-5")
pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))

storm <- storm[order(storm$TAU),]

storm$status <- paste(storm$TCDVLP, storm$SSNUM, sep='-')
storm$color <- as.character(factor(storm$status, levels = ss, labels = pal))
storm$advisory <- as.POSIXct(storm$ADVDATE, format='%y%m%d/%H%M')

title = paste("Advisory", storm$ADVISNUM[1], as.character(format(storm$advisory[1], "%b %d %H:%M")), "GMT", sep = " ")

m <- # create leaflet map
    leaflet(data=storm, width=1024, height=768) %>%
    addTiles() %>%
    addWMSTiles(
        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        layers = "nexrad-n0r-900913",
        options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>%
    addGeoJSON(ww, color = 'red', fill = FALSE) %>%
    addGeoJSON(radii, color = 'grey', opacity = 1, stroke = TRUE, weight = 1) %>%
    addGeoJSON(shp, stroke = TRUE, color = 'grey', fill = FALSE) %>%
    addGeoJSON(lin, weight = 2, fill = FALSE) %>%
    addCircles(lng = ~LON, lat = ~LAT, radius = ~SSNUM * 15000, color = ~color,
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

    ) %>%
    addLegend("topright", colors = pal, labels = ss, title = title)

html_print(m)
#saveWidget(m, '/var/www/html/trackr.html', selfcontained = FALSE)
