# Create Rdata from NOAA GIS shape files (http://www.nhc.noaa.gov/gis/)

library(geojsonio)
library(foreign)

url <- "http://www.nhc.noaa.gov/gis/forecast/archive/al142016_5day_latest.zip"

wd <- getwd()
td <- tempdir()
setwd(td)

temp <- tempfile(fileext = ".zip")
download.file(url, temp)
unzip(temp)

s <- dir(td, "*_5day_pgn.shp$")
l <- dir(td, "*_5day_lin.shp$")
d <- dir(td, "*_5day_pts.dbf$")

shp <- file_to_geojson(s, method='local', output=':memory:')
lin <- file_to_geojson(l, method='local', output=':memory:')

storm <- read.dbf(d)

unlink(dir(td))
unlink(temp)
setwd(wd)

save.image("NOAA_GIS.RData")
