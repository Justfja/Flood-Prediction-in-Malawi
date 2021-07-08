options(warn = -1)
library(sp)
library(rgdal)
library(gdalUtils)
library(aqp)

gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if (valid_install)
  print("Valid GDAL found") else
  {print("No valid GDAL"); quit()}

wfs <- "WFS:http://data.isric.org/geoserver/wosis_latest/wfs/"

layers.info <- ogrinfo(wfs, ro=TRUE, so=TRUE)
cat(layers.info, sep = "\n")

bd.india.info <- ogrinfo(wfs, ro=TRUE, so=TRUE, layer="wosis_latest_bdfi33",
  where="country_name='India' AND upper_depth > 100",
  verbose=TRUE)




wosis.dir.name <- "./wosis2019"
if (!file.exists(wosis.dir.name)) dir.create(wosis.dir.name)
zip.file.name <- "WoSIS_2019_September.zip"
snapshot.zip <- paste0("https://files.isric.org/public/wosis_snapshot/",
  zip.file.name)
target.zip <- paste0(wosis.dir.name, "/", zip.file.name)
if (!file.exists(target.zip)) {
  download.file(snapshot.zip, destfile=target.zip)
}