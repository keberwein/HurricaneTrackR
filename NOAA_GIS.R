# Create Rdata from NOAA GIS shape files (http://www.nhc.noaa.gov/gis/)

library(geojsonio)
library(foreign)
library(XML)

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

url <- links$link[grep('MATTHEW', links$title)]

temp <- tempfile(fileext = ".zip")
download.file(url, temp)
unzip(temp)

d <- dir(td, "*_5day_pts.dbf$")
l <- dir(td, "*_5day_lin.shp$")
p <- dir(td, "*_5day_pts.shp$")
s <- dir(td, "*_5day_pgn.shp$")

storm <- read.dbf(d)
lin <- file_to_geojson(l, method='local', output=':memory:')
pts <- file_to_geojson(p, method='local', output=':memory:')
shp <- file_to_geojson(s, method='local', output=':memory:')

rm (d, l, p, s, gis_at, gis_doc, links)
unlink(temp)
unlink(dir(td))
setwd(wd)

save.image("/tmp/NOAA_GIS.RData")
