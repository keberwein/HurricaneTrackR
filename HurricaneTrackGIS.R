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
library(geojsonio)
library(foreign)

stormname = "MATTHEW"

getCurrentAdv <- function(stormname) {
    gis_at <- read_xml("http://www.nhc.noaa.gov/gis-at.xml")
    gis_doc <- xmlParse(gis_at)
    links <- xmlToDataFrame(gis_doc, nodes=getNodeSet(gis_doc, "//item"))

    # keep only Advisory shapefile links
    links <- links[grep(paste("Advisory [#0-9A-Z]+ Forecast \\[shp\\] - [a-z A-Z]+", stormname, sep = " "), links$title),]

    adv <- regmatches(links$title, regexpr('#[0-9]+[A-Z]?', links$title))
    adv <- sub("#0?", "", adv)
    return(adv)
}

getStorm <- function(stormname) {
    print("Getting NOAA GIS data")
    wd <- getwd()
    td <- tempdir()
    setwd(td)

    gis_at <- read_xml("http://www.nhc.noaa.gov/gis-at.xml")
    gis_doc <- xmlParse(gis_at)
    links <- xmlToDataFrame(gis_doc, nodes=getNodeSet(gis_doc, "//item"))

    # keep only Advisory shapefile links
    links <- links[grep("Advisory [#0-9A-Z]+ Forecast \\[shp\\]", links$title),]
    # cleanup titles for menu
    links$title <- gsub('\\[shp\\] ', "", links$title)
    # add advisory number
    links$advisory <- regmatches(links$title, regexpr('#[0-9]+[A-Z]?', links$title))

    ## interactive storm selection
    #l <- select.list(links$title, title="Select storm:", graphics = FALSE)
    #links$link[links$title == l]

    url <- links$link[grep(stormname, links$title)]

    temp <- tempfile(fileext = ".zip")
    download.file(url, temp)
    unzip(temp)

    d <- dir(td, "*_5day_pts.dbf$")
    l <- dir(td, "*_5day_lin.shp$")
    p <- dir(td, "*_5day_pts.shp$")
    s <- dir(td, "*_5day_pgn.shp$")

    storm <<- read.dbf(d)
    lin <<- file_to_geojson(l, method='local', output=':memory:')
    pts <<- file_to_geojson(p, method='local', output=':memory:')
    shp <<- file_to_geojson(s, method='local', output=':memory:')

    rm (d, l, p, s, gis_at, gis_doc, links)
    unlink(temp)
    unlink(dir(td))
    setwd(wd)

    save.image("/tmp/NOAA_GIS.RData")
}

# load local GIS data to save time pulling infrequent GIS updates, but keep NEXRAD data current
adv <- getCurrentAdv(stormname)
if (!file.exists("/tmp/NOAA_GIS.Rdata")) {
    getStorm(stormname)
} else {
    load("/tmp/NOAA_GIS.Rdata")
    # repull GIS data if not current
    if (adv != storm$ADVISNUM[1]) {
        getStorm(stormname)
    } else {
        print("Using exsiting GIS data")
    }
}

# storm scale
ss <-  c("Tropical Depression", "Tropical Storm", "Hurricane-1", "Hurricane-2", "Major Hurricane-3", "Major Hurricane-4", "Major Hurricane-5")
pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))

storm <- storm[order(storm$TAU),]

storm$status <- paste(storm$TCDVLP, storm$SSNUM, sep='-')
storm$color <- as.character(factor(storm$status, levels = ss, labels = pal))
storm$advisory <- as.POSIXct(storm$ADVDATE, format='%y%m%d/%H%M')

m <- # create leaflet map.
    leaflet(data=storm, width=1024, height=768) %>%
    addTiles() %>%
    addWMSTiles(
        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        layers = "nexrad-n0r-900913",
        options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>%
    addGeoJSON(shp, stroke = TRUE, color = 'grey', fill = FALSE) %>%
    addGeoJSON(lin, fill = FALSE) %>%
    addCircleMarkers(~LON, ~LAT, radius = ~SSNUM * 4, stroke = TRUE, color = ~color,
                     opacity = 1, weight = 2, fill = TRUE, fillColor = ~color,
                     popup = ~sprintf("<b>Advisory forecast %s: %s</b><hr noshade size='1'/>
                                      Time: %s %s<br/>
                                      Position: %3.2f, %3.2f<br/>
                                      Status: <strong>%s</strong><br/>
                                      Wind: %s kts<br/>
                                      Gust: %s kts",
                                      htmlEscape(ADVISNUM), htmlEscape(format(advisory, "%b %d %H:%M")),
                                      htmlEscape(DATELBL), htmlEscape(TIMEZONE),
                                      htmlEscape(LON), htmlEscape(LAT),
                                      htmlEscape(status),
                                      htmlEscape(MAXWIND), htmlEscape(GUST))

    ) %>%
    addLegend("bottomright", colors = pal, labels = ss)

html_print(m)

saveWidget(m, '/tmp/trackr.html', selfcontained = TRUE)
