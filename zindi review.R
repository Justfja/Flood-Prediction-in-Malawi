options(warn = -1)

library(lightgbm)
library(reticulate)
library(plyr)
library(tidyverse)
library(caret)
library(SOAR)
library(data.table)
library(lubridate)
library(fastICA)
library(geosphere)
library(catboost)
library(xgboost)


#####
path.dir = getwd()
data.dir = paste0(path.dir,"/Data")

#dir.create(paste0(path.dir,"/subm"))
#dir.create(paste0(path.dir,"/tmp"))
#dir.create(paste0(path.dir,"/cv"))
save.files.dir = paste0(path.dir,"/tmp")
subm.dir = paste0(path.dir,"/subm")
cv.dir = paste0(path.dir,"/cv")

####  SOME FUNCTIONS 
#### my f2 cnt -- FOR COUNTING OCCURENCE
my.f2cnt = function(th2,vn1,vn2, filter = TRUE){
  data = data.frame(f1= th2[,vn1],f2=th2[,vn2], filter = filter)
  colnames(data) = c("f1","f2","filter")
  sum1 = sqldf::sqldf("select f1,f2, count(*) as cnt from data where filter=1 group by 1,2")
  tmp = sqldf::sqldf("select b.cnt from data a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp$cnt[is.na(tmp$cnt)] = 0
  return(tmp$cnt)
}
#### conect to SOAR package
fstoreconnect = function(subdir){
  oldLC = Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
  Sys.setenv(R_LOCAL_CACHe = subdir)
}
fstoreconnect("rstore")
tmp = Objects()


#### FEATURES
# LC_Type1_mode     
#  X                  
#  Y                  
#  elevation          
#  precip_week_5      
#  precip_week_6      
#  precip_week_7      
#  precip_week_8      
#  precip_week_9      
#  precip_week_10     
#  precip_week_11     
#  precip_week_12     
#  precip_week_13     
#  slope              
#  aspect             
#  flowdir            
#  tpi                
#  tri                
#  roughness          
#  NAME_2           - district name for adminstrative areas  
#  NAME_3           - subdistrict name for adminstrative areas  
#  alt              - altitiude 
#  msk_alt          - mask altitiude 
#  land_cover       - landcover  
#  temp             - temp  
#  dist_to_nsanje   - distance to nsanje
#  dist_to_shire    - distanceto shire reiver
#  dist_to_salima   -distance to salima 
#  LC_Type1_mode_X_cnt
#  elevation_X        - elevation * X
#  elevation_LC_Type  - elevation * LC_TYPE1_mode
#  X_Y                - X * Y
#  pw_rank1           - Rank of elevation by NAME_2(DISTRICT)
#  pw_rank2           - Rank of elevation by landcover
#  pw_rank3           -  max Rank of elevation by landcover


##### LOAD THE DATA
df = fread(paste0(data.dir,"/Train (2).csv")) %>% as.data.frame()
samp = fread(paste0(data.dir,"/SampleSubmission (1).csv"))

#####
label = df$target_2015
label2 = ifelse(label>0.5,1,0)
train.id = df$Square_ID
test.id = samp$Square_ID
Store(train.id)
Store(test.id)
Store(label)
Store(label2)


###### SPLIT DATA INFORMATION
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



#### INSTALL PACKAGES raster and sp


#### FEATURE ENINGERING FROM ELEVATION DATA
## GET SLOPE,ASPECT,FLOW DIRECTION,ROUGHNESS,TPI,TRI
dr.dat = data.frame(lon=df$X,lat = df$Y)
train_points = sp::SpatialPoints(dr.dat,sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#### USE ONLY THE CORRINATES AND ELEVATION DATA
d2 = data.frame(lon=df$X,lat = df$Y,elevation = df$elevation)
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

feat = raster::stack(slope,aspect,flowdir,TPI,TRI,Roughness)

## EXTRACT INFORMATION
b = raster::extract(feat,train_points)
df = cbind(df,b)


### GET ADMINSTRATIVE AREAS INFORMATION -- http://www.diva-gis.org/gdata
admin_areas = rgdal::readOGR(paste0(data.dir,"/SHP/MWI_adm3.shp"))
adm = raster::extract(admin_areas,train_points)
adm = adm %>% select(NAME_2,NAME_3)
df = cbind(df,adm)
### GET ALTITUDE INFORMATION -- http://www.diva-gis.org/gdata
alt = raster::raster(paste0(data.dir,"/SHP/MWI_alt.gri"))
df$alt = raster::extract(alt,train_points)

alt2 = raster::raster(paste0(data.dir,"/SHP/MWI_msk_alt.gri"))
df$msk_alt = raster::extract(alt2,train_points)

### GET Land cover INFORMATION -- SOURCE http://www.diva-gis.org/gdata
covr = raster::raster(paste0(data.dir,"/SHP/MWI_cov.gri"))
df$land_cover = raster::extract(covr,train_points)

### GET TEMP INFORMATION --SOURCE https://www.worldclim.org/data/worldclim21.html
temp = raster::raster(paste0(data.dir,"/SHP/wc2.1_10m_tavg_01.tif"))
df$temp = raster::extract(temp,train_points)

### GET DIsTANCE TO NEAREST CITIES
nsanje= cbind(lon =35.2533, lat = -16.9206)
salima = cbind(lon = 34.43,lat= -13.77)
shire_river = cbind(lon = 35.14, lat = -14.25)
## to NSANJE
df$dist_to_nsanje = distCosine(cbind(df[,"X"],df[,"Y"]), nsanje)
df$dist_to_shire = distCosine(cbind(df[,"X"],df[,"Y"]), shire_river)
df$dist_to_salima = distCosine(cbind(df[,"X"],df[,"Y"]), salima)

### EXTRACT FEARURE ENGINEERING
### DATA MANIPULATION
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
  dist_to_salima = dist_to_salima/10000,
  LC_Type1_mode_X_cnt = my.f2cnt(df,"LC_Type1_mode","X"),
  elevation_X = elevation * X,
  elevation_LC_Type = elevation * LC_Type1_mode,
  X_Y = X * Y
) %>% 
  select(-drop.cols)

df = df %>% group_by(NAME_2) %>% mutate(pw_rank1 = frank(elevation,ties.method = "dense")) %>% ungroup()
df = df %>% group_by(land_cover) %>% mutate(pw_rank2 = frank(elevation,ties.method = "dense")) %>% ungroup()
df = df %>% group_by(land_cover) %>% mutate(pw_rank3 = frank(elevation,ties.method = "max")) %>% ungroup()


### clear some unnecessary data from the environment
# rm(b,area,s,adm,covr,temp,nsanje,slop,aspect,flowdir,TPI,TRI,Roughnes,
#   rast,feat)


########
## SPLIT DATA INTO TRAINING AND TEST DATA
#######
df_train = df[1:length(train.id),]
df_test = df[(length(train.id)+1):nrow(df),]


### SELECT FEATURES
feature1 = c(colnames(df[,1:26]),"LC_Type1_mode_X_cnt","dist_to_shire")
regression_feat = c(colnames(df[,1:4]),paste0("precip_week_",5:13),
                    "dist_to_nsanje","LC_Type1_mode_X_cnt","dist_to_shire")
feature2 = c(colnames(df[,1:13]),colnames(df[,30:32]),"NAME_2","NAME_3",'land_cover',"pw_rank1","pw_rank2","pw_rank3","dist_to_nsanje","dist_to_shire","dist_to_salima")

gc();gc()

####### MODELLING

##### CLASSIFICATION MODELS
#### MODEL 1   XGBOOST                  
dtrain = xgb.DMatrix(as.matrix(df_train[,feature2]), label=label2)
dtest = xgb.DMatrix(as.matrix(df_test[,feature2]))

param = list(booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.01,
  colsample_bytree = 1,
  max_depth = 4,
  min_child_weight = 1,
  nthread = 4,
  gamma = 0,
  subsample = 0.8
)
set.seed(1235)
mod.xgb = xgb.train(data = dtrain,params = param,nrounds = 1000)
pred= predict(mod.xgb,dtest)
xgb_class = pred
sub = cbind(test.id,xgb_class)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/xgb_classifaction.csv"),row.names = F)



### MODEL 2 - LIGHTGBM             
df_train = cbind(df_train,label)
df_train = df_train[1:12000,]
label3 = df_train$label
df_train$label = NULL

devresult = rep(0,nrow(df_train))
predte = rep(0,nrow(df_test))
cvscore = c()
int.seed = c(500)

for (i in 1:length(int.seed)) {
  cat("model training",i,"\n")
  
  set.seed(int.seed[i])
  folds = createFolds(label3, k = 5)
  
  param = list(objective = "regression_l2",
    metric = "rmse",
    feature_fraction = 0.5,
    bagging_freq = 1,
    bagging_fraction = 0.8
  )
  
  for (this.round in 1:length(folds)) {
    cat("model training",i," ","fold ",this.round,"\n")
    valid = c(1:length(label3))[unlist(folds[this.round])]
    dev = c(1:length(label3))[unlist(folds[1:length(folds)!= this.round])]
    
    dtrain = lgb.Dataset(data = as.matrix(df_train[dev,regression_feat]),
      label = label3[dev], free_raw_data = F)
    dvalid = lgb.Dataset(data = as.matrix(df_train[valid,regression_feat]),
      label = label3[valid],free_raw_data= F)
    
    model = lgb.train(data = dtrain,
      params = param,
      nrounds = 1000,
      valids = list(val1 = dvalid, val2 = dtrain),
      boosting_type = "gbdt",
      learning_rate = 0.01,
      max_depth = -1,
      num_leaves = 13,
      num_threads = 4,
      eval_freq = 500,
      seed = 1235,
      verbose = 1,
      early_stopping_rounds = 20
    )
    
    pred = predict(model,as.matrix(df_train[valid,regression_feat]))
    devresult[valid] = pred
    pred_test = predict(model, as.matrix(df_test[,regression_feat]))
    predte = predte + pred_test
    
    cat("model cv rmse score:", model$best_score,"\n")
    cvscore = c(cvscore, model$best_score)
    cat("model cv rmse mean score:",mean(cvscore), "\n")
  }
}
pred = predte/5
pred = ifelse(pred<0.1,0,pred)
lgb_reg = pred
sub = cbind(test.id,lgb_reg)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/lgb_regression.csv"),row.names = F)

##### MODEL 3   CATBOOST
df_train = df[1:length(train.id),]
df_test = df[(length(train.id)+1):nrow(df),]


devresult = rep(0,nrow(df_train)) ## train oof
p = rep(0, nrow(df_test)) ### test prediction
dtest = catboost.load_pool(as.matrix(df_test[,feature1]))

for (i in 1) {
  
  print(paste0("model training on label ", i))
  
  
  for(rnd in 1:length(folds)){
    valid = c(1:length(label))[unlist(folds[rnd])]
    dev = c(1:length(label))[unlist(folds[1:length(folds) != rnd])]
    
    dtrain = catboost.load_pool(as.matrix(df_train[dev,feature1]), label=label2[dev])
    dvalid = catboost.load_pool(as.matrix(df_train[valid,feature1]),label=label2[valid])
    
    params = list(
      iterations = 5000,
      learning_rate = 0.01,
      depth = 3,
      eval_metric = "Logloss",
      loss_function = "Logloss",
      random_seed = 1235,
      use_best_model = TRUE,
      logging_level = "Verbose",
      rsm = 1,
      od_type = "Iter",
      od_wait = 50,
      metric_period = 2000
    )
    
    model = catboost.train(learn_pool= dtrain,test_pool= dvalid,params = params)
    predt = catboost.predict(model,
      pool = catboost.load_pool(as.matrix(df_train[valid,feature1])),
      prediction_type = "Probability")
    devresult[valid] = predt
    
    pred = catboost.predict(model, pool = dtest, 
      prediction_type = "Probability")
    
    p = p + pred
  }   
}

pred= p/5
cat_class = pred
sub = cbind(test.id,cat_class)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/catboost_classifaction.csv"),row.names = F)



##### FINAL ENSEMBLES AND PROCESSING
ensemble1 = 0.7*cat_class+ 0.3*xgb_class
final = 0.7*ensemble1 + 0.3*lgb_reg


sub = cbind(test.id,final)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/final.csv"),row.names = F)

### FINAL PROCESSING 
final = data.frame(id = sub[,1],target_2019 =final,label =label) 
final$process1 = ifelse(final$label>0.85 &final$label< 1& final$target_2019>0.80 & final$target_2019<0.9,1,0)
final$process2 = ifelse(final$label>0.85 &final$label< 1& final$target_2019>0.6 & final$target_2019<0.8,1,0)

#### set as 0.75
final2 = ifelse(final$process1==1,0.75,final$target_2019)
final2 = ifelse(final$process2==1,0.75,final2)

sub = cbind(test.id,final2)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/final2.csv"),row.names = F)
