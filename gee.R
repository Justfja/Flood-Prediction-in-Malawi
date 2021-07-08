
library(RStoolbox)
library(raster)
library(doSNOW)
library(doParallel)
library(parallel)
library(caret)
library(ranger)
library(randomForest)

##### RASTER PCA
# Time the code execution
start.time <- Sys.time()
# Create a cluster to work on 10 logical cores.
cl <- makeCluster(8, type = "PSOCK")
registerDoParallel(cl)
# 
 o = rasterPCA(feat)
 feat_s = raster::extract(o$map, train_points)
# set.seed(1235)
# mod.ran = caret::train(x = df_train,y = as.factor(label3), method = "ranger",
#   metric = "logLoss",
#   trControl = trainControl(
#     method = "cv",
#     number = 5,
#     classProbs = T,
#     verboseIter = T,
#     summaryFunction = mnLogLoss
#     #savePredictions = "final"
#   ))

stopCluster(cl)
Sys.time() -  start.time


# 
# ### Temp
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2015-01-01", "2015-02-01")$
#   map(ee_pyfunc(function(x) x$select("SoilTemp00_10cm_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# t =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(t) = c('NAME_2','temp_m1')
# 
# 
# ###
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2014-12-01", "2015-02-01")$
#   map(ee_pyfunc(function(x) x$select("Rainf_f_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# r =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(r) = c('NAME_2','rain_m1',"rain_m2")
# 
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2014-12-01", "2015-02-01")$
#   map(ee_pyfunc(function(x) x$select("Qair_f_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# q =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(q) = c('NAME_2','qair_m1','qair_m2')
# 
# ####
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2014-12-01", "2015-02-01")$
#   map(ee_pyfunc(function(x) x$select("Wind_f_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# w =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(w) = c('NAME_2','wind_m1',"wind_m2")
# 
# ####
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2014-12-01", "2015-02-01")$
#   map(ee_pyfunc(function(x) x$select("Psurf_f_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# w2 =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(w2) = c('NAME_2','sp_m1','sp_m2')
# 
# 
# terraclimate <- ee$ImageCollection("JRC/GSW1_1/YearlyHistory")$
#   #filterDate("2014-12-01", "2015-02-01")$
#   map(ee_pyfunc(function(x) x$select("waterClass")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# wt =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(wt) = c('NAME_2','2014',"2015","2016","2017","2018")
# wt = wt %>% select(NAME_2,`2015`,`2018`)
# 
# 
# 
# 
# terraclimate <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
#   filterDate("2015-01-10", "2015-01-14")$
#   map(ee_pyfunc(function(x) x$select("precipitation")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# p1 =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# #colnames(p1) = c('NAME_2','sp_m1',"sp_m2")
# 
# terraclimate <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
#   filterDate("2019-03-11", "2019-03-15")$
#   map(ee_pyfunc(function(x) x$select("precipitation")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# p2 =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# 
# 
# 
# 
# #### TRMM 3B
# terraclimate <- ee$ImageCollection("TRMM/3B42")$
#   filterDate("2015-01-12", "2015-01-14")$
#   map(ee_pyfunc(function(x) x$select("precipitation")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# p3 =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# 
# 
# terraclimate <- ee$ImageCollection("MODIS/006/MCD12Q1")$
#  filterDate("2017-01-01", "2018-01-01")$
#   map(ee_pyfunc(function(x) x$select("LC_Type2")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = s2[1:100,], fun = ee$Reducer$max(), id = "NAME_1")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$max(), id = "NAME_1")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$max(), id = "NAME_1")
# p4 =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### Temp
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2019-03-01", "2019-04-01")$
#   map(ee_pyfunc(function(x) x$select("SoilTemp00_10cm_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# t =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(t) = c('NAME_2','temp_m19')
# 
# 
# ###
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2019-03-01", "2019-04-01")$
#   map(ee_pyfunc(function(x) x$select("Rainf_f_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# r =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(r) = c('NAME_2','rain_m19')
# 
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2019-03-01", "2019-04-01")$
#   map(ee_pyfunc(function(x) x$select("Qair_f_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# q =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(q) = c('NAME_2','qair_m19')
# 
# ####
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2019-03-01", "2019-04-01")$
#   map(ee_pyfunc(function(x) x$select("Wind_f_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# w =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(w) = c('NAME_2','wind_m19')
# 
# ####
# terraclimate <- ee$ImageCollection("NASA/FLDAS/NOAH01/C/GL/M/V001")$
#   filterDate("2019-03-01", "2019-04-01")$
#   map(ee_pyfunc(function(x) x$select("Psurf_f_tavg")))
# 
# ee_nc_rain <- ee_extract(x = terraclimate, y = head(s2,100), fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain2 <- ee_extract(x = terraclimate, y = s2[101:200,], fun = ee$Reducer$mean(), id = "NAME_2")
# ee_nc_rain3 <- ee_extract(x = terraclimate, y = s2[201:nrow(s2),], fun = ee$Reducer$mean(), id = "NAME_2")
# w2 =rbind(ee_nc_rain,ee_nc_rain2,ee_nc_rain3)
# colnames(w2) = c('NAME_2','sp_m19')
# 
# 
# Tr_Daily_precip <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
#   filterDate("2015-01-13", "2015-01-14")$reduce(reducer = ee$Reducer$mean())
# 
# 
# #### TRMM 3B
# pre <- ee$ImageCollection("TRMM/3B42")$
#   filterDate("2015-01-13", "2015-01-14")$reduce(reducer = ee$Reducer$mean())
# path <- pre$getDownloadUrl(geom_params)
# print(path)
# 
# 
#### landcover modis
modis <- ee$ImageCollection("MODIS/006/MCD12Q1")$
  filterDate("2017-01-01", "2018-01-01")$reduce(reducer = ee$Reducer$max())
path <- modis$getDownloadUrl(geom_params)
print(path)

###ndwi
ndwi <- ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global5")$
  filterDate("2015-01-01", "2019-01-01")$
  map(ee_pyfunc(function(x) x$select("data-density-indicator")))$reduce(reducer = ee$Reducer$mean())
path <- ndwi$getDownloadUrl(geom_params)
print(path)

# # Method 01: Get a download URL for an image.
# image1 <- ee$Image('srtm90_v4')
geom_params <-   list(
 scale = 100,
  #crs = 'EPSG:4326',
  #region = '[[32.67, -17.12], [35.91, -9.37], [35.91, -9.36], [32.67, -17.11]]',maxPixels = 10000,
  dimensions = 1000
)
# path <- image1$getDownloadUrl(geom_params)
# print(path)
# 
# 
image2 = ee$Image('JRC/GHSL/P2016/BUILT_LDSMT_GLOBE_V1')$
  filterDate("2005-01-01", "2014-01-01")$reduce(reducer = ee$Reducer$mean())
path <- image2$getDownloadUrl(geom_params)
print(path)


s = rgdal::readOGR(paste0(data.dir,"/Africa_Boundaries.shp"))

s = sf::st_read(paste0(data.dir,"/Africa_Boundaries.shp"))
ee_nc_rain2 <- ee_extract(x = image2, y = s, fun = ee$Reducer$mean(),scale =100)




terraclimate <- ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>% 
  ee$ImageCollection$filterDate("2018-01-01", "2019-01-01") %>% 
  ee$ImageCollection$map(function(x) x$select("discrete_classification")) %>% 
  ee$ImageCollection$toBands() %>% 
  ee$Image()#$rename(sprintf("%02d",1:12))
path <- terraclimate$getDownloadUrl(geom_params)
print(path)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
ee_nc_rain <- ee_extract(x = terraclimate, y = s["NAME_0"], sf = FALSE,scale= 100)
