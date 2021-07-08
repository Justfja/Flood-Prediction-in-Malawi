options(warn = -1)

library(lightgbm)
library(reticulate)
library(raster)
library(plyr)
library(tidyverse)
library(caret)
library(SOAR)
library(data.table)
library(lubridate)
library(fastICA)
library(geosphere)



#####
path.dir = getwd()
data.dir = paste0(path.dir,"/Data")

#dir.create(paste0(path.dir,"/subm"))
save.files.dir = paste0(path.dir,"/tmp")
subm.dir = paste0(path.dir,"/subm")

####
source("utils.R")


#####
df = fread(paste0(data.dir,"/Train (2).csv")) %>% as.data.frame()

#
label = df$target_2015
train.id = df$Square_ID
test.id = samp$Square_ID
Store(train.id)
Store(test.id)
Store(label)

df = df %>% within(rm("target_2015"))
df2 = df %>% select(Square_ID,LC_Type1_mode,X,Y,elevation)
test = bind_cols(df2,df %>% select(contains("2019")))
df = bind_cols(df2,df %>% select(contains("2014")),
                df %>% select(contains("2015")))

rm(df2)
df$`precip 2014-12-28 - 2015-01-041`=NULL

####
colnames(df) = c(colnames(df[,1:5]),paste0("precip_week_",1:ncol(df[,6:ncol(df)])))
colnames(test) = c(colnames(test[,1:5]),paste0("precip_week_",1:ncol(test[,6:ncol(test)])))
df = bind_rows(df,test)



#### FEATURE ENINGERING FROM ELEVATION DATA
dr.dat = data.frame(lon=df$X,lat = df$Y)
train_points = sp::SpatialPoints(dr.dat,sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#### USE ONLY THE CORRINATES AND ELEVATION DATA
d2 = data.frame(lon=df$X,lat = df$Y,elevation = round(df$elevation))
rast <- raster::rasterFromXYZ(d2)
raster::crs(rast) = c("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#### GET ELEVATION DATA - SOURCE http://www.masdap.mw/layers/geonode:malawi_srtm30meters
srtm = raster::raster(paste0(data.dir,"/SHP/malawi_srtm30meters.tif"))

####
slope= raster::terrain(srtm,opt = 'slope', unit = 'degrees',neighbors = 8)
aspect =raster::terrain(srtm, opt = 'aspect', unit = 'degrees',neighbors = 8)
flowdir = raster::terrain(srtm, opt = 'flowdir')
TPI = raster::terrain(srtm, opt = 'TPI')
TRI = raster::terrain(srtm, opt = 'TRI')
Roughness = raster::terrain(srtm, opt = 'roughness')
#area = raster::area(rast)
feat = raster::stack(slope,aspect,flowdir,TPI,TRI,Roughness)

## EXTRACT INFORMATION
b = raster::extract(feat,train_points)
df = cbind(df,b)
#df$area = raster::extract(area,train_points,method ="bilinear")


### GET ADMINSTRATIVE AREAS INFORMATION -- http://www.diva-gis.org/gdata
s = rgdal::readOGR(paste0(data.dir,"/SHP/MWI_adm3.shp"))
adm = raster::extract(s,train_points)
adm = adm %>% select(poly.ID,NAME_2,NAME_3)
df = cbind(df,adm)
### GET ALTITUDE INFORMATION -- http://www.diva-gis.org/gdata
alt = raster::raster(paste0(data.dir,"/SHP/MWI_alt.gri"))
df$alt = raster::extract(alt,train_points)

alt2 = raster::raster(paste0(data.dir,"/SHP/MWI_msk_alt.gri"))
df$msk_alt = raster::extract(alt2,train_points)

### GET Land cover INFORMATION -- SOURCE http://www.diva-gis.org/gdata
covr = raster::raster(paste0(data.dir,"/SHP/MWI_cov.gri"))
df$covr = raster::extract(covr,train_points)
#df$covr2 = raster::extract(covr,train_points, method = "bilinear")

### GET TEMP INFORMATION --SOURCE https://www.worldclim.org/data/worldclim21.html
temp = raster::raster(paste0(data.dir,"/SHP/wc2.1_10m_tavg_01.tif"))
df$temp = raster::extract(temp,train_points)

### GET DIsTANCE TO NEAREST CITIES
nsanje= cbind(lon =35.2533, lat = -16.9206)
chikwawa = cbind(lon = 34.80, lat = -16.04) 
zomba = cbind(lon = 35.33,lat = -15.37)
blantyre = cbind(lon = 35.01, lat = -15.76)
mulanje = cbind(lon = 35.50,lat = -16.02)
thyolo = cbind(lon= 35.14,lat = -16.06)
phalombe = cbind(lon = 35.65,lat = -15.76)
mangochi = cbind(lon= 35.25,lat = -14.48)
salima = cbind(lon = 34.43,lat= -13.77)
beira = cbind(lon = 34.8389,lat = -19.8436)
#lake_lamobe = cbind(lon = 35.23, lat= -14.64)
lake_chilwa = cbind(lon = 35.71,lat = -15.27)
shire_river = cbind(lon = 35.14, lat = -14.25)
## to NSANJE
df$dist_to_nsanje = distCosine(cbind(df[,"X"],df[,"Y"]), nsanje)/10000
# df$dist_to_malombe = distCosine(cbind(df[,"X"],df[,"Y"]), lake_lamobe)
#df$dist_to_chilwa = distCosine(cbind(df[,"X"],df[,"Y"]), lake_chilwa)/10000
# df$dist_to_chikwawa = distCosine(cbind(df[,"X"],df[,"Y"]), chikwawa)/10000
# df$dist_to_zomba = distCosine(cbind(df[,"X"],df[,"Y"]), zomba)/10000
# df$dist_to_blantyre = distCosine(cbind(df[,"X"],df[,"Y"]), blantyre)/10000
# df$dist_to_mulanje = distCosine(cbind(df[,"X"],df[,"Y"]), mulanje)/10000
# df$dist_to_thyolo = distCosine(cbind(df[,"X"],df[,"Y"]), thyolo)/10000
# df$dist_to_phalombe = distCosine(cbind(df[,"X"],df[,"Y"]), phalombe)/10000
#df$dist_to_mangochi = distCosine(cbind(df[,"X"],df[,"Y"]), mangochi)/10000
df$dist_to_shire = distCosine(cbind(df[,"X"],df[,"Y"]), shire_river)/10000
df$dist_to_salima = distCosine(cbind(df[,"X"],df[,"Y"]), salima)/10000
#df$dist_to_beira = distCosine(cbind(df[,"X"],df[,"Y"]), beira)/10000

#df$dist_to_shire2 = distHaversine(cbind(df[,"X"],df[,"Y"]), shire_river,r = 6371 )/10000

# df$dist = distCosine(cbind(df[,"X"],df[,"Y"]),cbind(q[,2],q[,1]))
# 
# df$bearing = bearing(cbind(df[,"X"],df[,"Y"]),shire_river)
# 
# df$bearing2 = bearing(cbind(df[,"X"],df[,"Y"]),nsanje)


##http://geoportal.rcmrd.org/layers/servir%3Amalawi_national_precipitation_trend
t2 = raster::raster(paste0(data.dir,"/New folder/malawi_national_precipitation_trend.tif"))


### EXTRA FEARURE ENGINEERING &  DATA MANIPULATION

drop.cols = c("Square_ID",paste0("precip_week_",1:4),
  paste0("precip_week_",14:17))
#####
df = df %>% mutate(
  NAME_2 = as.numeric(NAME_2),
  NAME_3 = as.numeric(NAME_3),
  slope = ifelse(is.na(slope),-1,slope),
  aspect = ifelse(is.na(aspect),-1,aspect),
  flowdir = ifelse(is.na(flowdir),-1,flowdir),
  tri = ifelse(is.na(tri),-1,tri),
  tpi = ifelse(is.na(tpi),-1,tpi),
  roughness = ifelse(is.na(roughness),-1,roughness),
  NAME_2 = ifelse(is.na(NAME_2),-1,NAME_2),
  NAME_3 = ifelse(is.na(NAME_3),-1,NAME_3),
  msk_alt = ifelse(is.na(msk_alt),-1,msk_alt),
  dist_to_nsanje = dist_to_nsanje/10000,
  dist_to_shire = dist_to_shire/10000,
  LC_Type1_mode_X_cnt = my.f2cnt(df,"LC_Type1_mode","X")
) %>% 
  select(-drop.cols)

### clear some unnecessary data from the environment
rm(b,area,s,adm,covr,temp,nsanje,slop,aspect,flowdir,TPI,TRI,Roughness,
  rast,feat,alt2,alt,shire_river)


df$b = df$X*df$Y
df$b2 = sqrt(df$elevation)*df$X
df$b3 = df$X/df$Y

df = df %>% group_by(NAME_2) %>% mutate(pwr = frank(elevation,ties.method = "dense")) %>% ungroup()

df = df %>% group_by(covr) %>% mutate(pwr2 = frank(elevation,ties.method = "dense")) %>% ungroup()
df = df %>% group_by(covr) %>% mutate(pwr3 = frank(elevation,ties.method = "max")) %>% ungroup()

df = df %>% group_by(covr) %>% mutate(mm = mean(label,na.rm=T)) %>% ungroup()


########
## SPLIT DATA INTO TRAINING AND TEST DATA
#######

df_train = df[1:length(train.id),]
df_test = df[(length(train.id)+1):nrow(df),]

rm(df,test)

gc();gc()

########
########
df= df %>% 
   add_count(X) %>%
   rename("X_cnt" = n) %>%

  
transform = preProcess(df, method = c("center","scale"))
#print(object.size(a), units = "Mb")

# der = function(x,d=1){
#     df = t(diff(t(x), differences = d))
#     return(df)
# }
# 
# frac = 0.3
# frac2 = 0.5
# frac3 = 1
# set.seed(6043)
# dt = rbindlist(list(df[is.na(label) | label==1], 
#   df[label==0][sample(1:.N,.N*frac2)]))
a = gh_encode(df$Y,df$X,4)
a2 = gh_neighbors(a)
k = matrix(unlist(a2), nrow = nrow(df),ncol = 9)
colnames(k) = names(a2)
#### EFFECTS -- NOT USED
library(lme4)

effect = list()
for (i in unique(tr$covr)){
  glmm = glmer(I(label2) ~ ( 1 | elevation), 
    data = tr[tr$covr == i,],family = "binomial")
  random_effects = ranef(glmm)$covr
  effect[[i]] = data.frame(covr = i, elevation = as.numeric(rownames(random_effects)), effect_cv = random_effects[,"(Intercept)"])
}

effect = do.call(rbind,effect)
  
  ##### CHECK DATA PROPERTIES
  ftrs = data.frame(
    type = unlist(lapply(df[1:length(train.id),],class)),
    n.unique = unlist(lapply(df[1:length(train.id),],function(x)length(unique(x)))),
    f.missing = unlist(lapply(df[1:length(train.id),],function(x)mean(is.na(x)))),
    spear.cor = unlist(lapply(df[1:length(train.id),],function(x){idx = !is.na(x);
    if(is.factor(x)) x = as.numeric(x);
    if(is.character(x)) x = as.numeric(as.factor(x));
    if(is.integer(x)) x = as.numeric(x);
    if(is.logical(x)) x = as.numeric(x);
    cor(x[idx],y = label[idx], method = "spearman")
    }))
  )

ftrs$name = rownames(ftrs)
ftrs =ftrs %>% drop_na()
df = df[,names(df) %in% ftrs$name]



transform = preProcess(dr.dat, method = c("center","scale","pca"))
pc = predict(transform, dr.dat) %>% as.data.frame()
df$lat_pca = pc$PC1
df$lon_pca = pc$PC2

######### KEMEANS CLUSTERING AND DISTANCE - NOT USED
dr.dat = cbind(X = df$X,Y = df$Y)
dr.dat = scale(dr.dat)
sumsq = NULL
for (i in 1:15) {
  set.seed(1234)
 sumsq[i] = sum(kmeans(dr.dat,centers = i, iter.max = 1000,
                       algorithm = "Forgy")$withinss)
}
plot(1:15,sumsq,type= "b")
###
set.seed(1234)
kmns = kmeans(dr.dat,4,nstart = 1,iter.max = 1000,
              algorithm = "Forgy",trace = T)

cnts = kmns$centers
kmeans.distance = NULL
for (i in 1:nrow(cnts)) {
  kmeans.distance = cbind(kmeans.distance, sqrt(colSums((t(dr.dat)-unlist(cnts[i,]))^2)))

}
table(kmns$cluster)
colnames(kmeans.distance)= paste0("knn_dist",1:ncol(kmeans.distance))

save(kmeans.distance, file = paste(save.files.dir,"/kmeans_features.RData"))



##### CHECK DUPLICATED FEAURES
duplicated.x = findCorrelation(cor(randomForest::na.roughfix(df)),
                               cutoff = 0.99, names = T, verbose = F)
length(duplicated.x)
df_train = df_train[, !names(df_train) %in% duplicated.x]

df = df[, !names(df) %in% duplicated.x]



##### TSNE CLUSTERING -- NOT USED
library(Rtsne)
set.seed(1234)
tsne.res = Rtsne(df, check_duplicates = F, max_iter=1000, perplexity = 50,
                 theta = 0.5, dims = 2, verbose =T)
save(tsne.res, file = paste0(save.files.dir,"/tsne.RData"))

#### FEARURE SSVD DECOMPOSTION-- NOT USED
library(irlba)
set.seed(1234)
s = ssvd(as.matrix(df), k = 4,n =10,maxit = 1000)
colnames(s$u) = paste0("svd",1:ncol(s$u))
svd.features = data.frame(s$u)


# ONE HOT ENCODING FOR CATEGORICAL VARIABLES -- NOT USED
# get names of categorical features
df.categories = df %>% select(LC_Type1_mode)

# one hot encoding for categorical data
dummy <- dummyVars(" ~ .",data=df.categories)
df.categoric <- data.frame(predict(dummy,newdata=df.categories))
df = df %>% cbind(df.categoric) %>% select(-LC_Type1_mode)
# check for near-zero variance
nzv.data <- nearZeroVar(df, saveMetrics = TRUE)
# take any of the near-zero-variance perdictors
drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]
df <- df[,!names(df) %in% drop.cols]
paste('The dataframe now has', dim(df)[1], 'rows and', dim(df)[2], 'columns')



for(i in 1:ncol(df)){
  df[,i] = as.numeric(df[,i])
}

##### UMAP COMPONENTS---  NOT USED
data3 = cbind(df[,2:4])
um = umap::umap(dr.dat)
colnames(um$layout) = paste0("umap",1:ncol(um$layout))


# k nearest neighbors -- NOT USED
d = cbind(dr.dat$lon, dr.dat$lat)
k <- 8
nn <- knearneigh(d, k)
orstationc.neighbors.knn <- knn2nb(nn)
plot(orstationc.neighbors.knn, d, add=TRUE, col="green")

knn.feature = matrix(unlist(orstationc.neighbors.knn), ncol = 8)

### DBSCAN - NOT USED
set.seed(1234)
# fpc package
res.fpc <- fpc::dbscan(df$elevation, eps = 4, MinPts = 8)
summary(as.factor(res.fpc$cluster))
save(res.fpc, file = paste0(save.files.dir,"/dbscan_clus.RData"))



## analyzing spatial data
leaflet(data = tr2) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(~X, ~Y, radius = 0.1,
    color = "blue", fillOpacity = 0.3)

d = 


s = rgdal::readOGR(paste0(data.dir,"/External/mwi_nhr_naturalshocksrisk_geonode_20140612.shp"))
s = rgdal::readOGR(paste0(data.dir,"/External/wetlands_100.shp"))
s = rgdal::readOGR(paste0(data.dir,"/External/agroclimatic_zonesgeo.shp"))
s = rgdal::readOGR(paste0(data.dir,"/New folder/malawi_adm3.shp"))
b = raster::extract(s,train_points)
# b = b %>% select(ID_1,NAME_2,NAME_3)


raster::crs(s) = c("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# s2 = rgdal::readOGR(paste0(data.dir,"/SHP/MWI_adm3.dbf"))

t = raster::raster(paste0(data.dir,"/SHP/CHELSAcruts_prec_1_2015_V.1.0.tif"))
# 
# 
t3 = raster::raster(paste0(data.dir,"/External/degradation_biodiversity.tif"))
# 
t3 = raster::raster(paste0(data.dir,"/External/HYP_LR/HYP_LR.tif"))
# 
t3 = raster::raster(paste0(data.dir,"/External/malawi_national_soil_organic_carbon0.tif"))

t2 = raster::raster(paste0(data.dir,"/SHP/srtm90_v4.elevation.tif"))
t2 = raster::raster(paste0(data.dir,"/SHP/MWI_pop.gri"))


t2 = raster::stack(paste0(data.dir,"/download.precipitation_mean.tif"),
            paste0(data.dir,"/download.HQprecipitation_mean.tif"),
            paste0(data.dir,"/download.IRprecipitation_mean.tif"))

t2 = raster::raster(paste0(path.dir,"/download2.nc"))

t2 = raster::raster(paste0(data.dir,"/rfe2019_03-dk1_anom.nc"))




###### modis
i = list.files(path = paste0(data.dir), pattern = "*.*tif")
y = raster::stack(paste0(data.dir,"/",i))
m = raster::extract(y,train_points)


t2 = raster::raster(paste0(data.dir,"/New folder/servir_malawi_landcover_2000_scheme_i.tif"))

t2 = raster::raster(paste0(data.dir,"/New folder/malawi_national_precipitation_trend.tif"))


s = rgdal::readOGR(paste0(data.dir,"/New folder2/TA_admin_affected_population.shp"))

library(raster) # load the raster package
brk <- do.call(brick, lapply(list.files(path = paste0(data.dir,"/SHP/Rainfall/train/"), pattern = "*.*tif"), raster))
i = list.files(path = paste0(data.dir,"/SHP/Rainfall/train"), pattern = "*.*tif")
y = stack(paste0(data.dir,"/SHP/Rainfall/train/",i))
m = extract(y,train_points)

for(i in 1:length(seq(1,119,7))){
  j = seq(1,119,7)
  x = j[i]-1
  y = 7*i
  n[,i+1] = rowSums(m[,x:y])
}


n[,2] = rowSums(m[,1:7])


df$vegetation = ifelse(df$nn %in% c('Balaka','Chikwawa','Nsanje'),1,
  ifelse(df$nn %in% c('Mwanza','Mulanje','Zomba','Machinga','Thyolo'),2,
  ifelse(df$nn %in% c('Blantyre','Ntcheu'),3,
    ifelse(df$nn %in% c('Phalombe','Chiradzulu'),4,
      ifelse(df$nn %in% c('Neno'),5,-1)))))


df$soil = ifelse(df$nn %in% c('Balaka','Chikwawa'),1,
  ifelse(df$nn %in% c('Machinga','Thyolo'),2,
    ifelse(df$nn %in% c('Blantyre','Ntcheu','Zomba'),3,
      ifelse(df$nn %in% c('Phalombe','Chiradzulu'),4,
        ifelse(df$nn %in% c('Nsanje'),5,
          ifelse(df$nn %in% c('Mwanza'),6,
            ifelse(df$nn %in% c('Mulanje'),7,
        ifelse(df$nn %in% c('Neno'),8,-1))))))))


df$w = ifelse(df$NAME_1 %in% c('Mwanza',"Zomba",'Phalombe',"Chikwawa"),1,0)

library(woeBinning)
tr = cbind(df_train,label2)
bin_vars = c("elevation","alt")
binning = woe.binning(tr,"label2",bin_vars)

tr = woe.binning.deploy(tr, binning, add.woe.or.dum.var = "woe") %>% 
  select(~ !is.factor(.))




### fast knn
set.seed(2019)
cv = fastknnCV(as.matrix(df_train), as.factor(label2),
              k = seq(2,20,2),
              method = "dist",
              folds = 5,
              eval.metric = "logloss",
              nthread = 8)
pander::pander(cv$cv_table)
####
set.seed(2020)
knn.feature = knnExtract(xtr = as.matrix(df_train),
                         ytr = as.factor(label2),
                         xte = as.matrix(df_test),
                         nthread = 8,
                         k =10)
save(knn.feature, file = paste(save.files.dir,"/kmeans_features.RData"))



#m = cbind(xccsub2,label)
m = data.frame(id = sub[,1],target_2019 =m2,label =label) 
m$rr = ifelse(m$label==1 & m$target_2019>0.6 &m$target_2019<0.9,1,0)#0.85
m$rr2 = ifelse(m$label <0.3 & m$target_2019>0.5,1,0)#0.8
m$rr4 = ifelse(m$label>0.85 &m$label< 1& m$target_2019>0.80 & m$target_2019<0.9,1,0)#0.8
m$rr5 = ifelse(m$label>0.85 &m$label< 1& m$target_2019>0.6 & m$target_2019<0.8,1,0)#0.8


#m$rr4 = ifelse(m$label>0.9&m$target_2019>0.4 & m$target_2019<0.5,1,0)
m$rr6 = ifelse(m$label>0.75 &m$label< 0.85 & m$target_2019>0.7 &m$target_2019<0.8,1,0)#0.8

m4 = ifelse(m$rr==1,0.8,m$target_2019)
m4 = ifelse(m$rr2==1,0.7,m4)
m4 = ifelse(m$rr4==1,0.75,m4)
m4 = ifelse(m$rr5==1,0.75,m4)
m4 = ifelse(m$rr6==1,0.9,m4)

#####
library(NbClust)
res.nb <- NbClust(df_train, distance = "euclidean",
  min.nc = 2, max.nc = 10, 
  method = "complete", index ="all") 


train_balance <- SMOTE(label2 ~ .,data = df_train)
