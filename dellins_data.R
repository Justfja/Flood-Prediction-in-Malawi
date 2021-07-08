####libraries
library(plyr)
library(tidyverse)

#####
path = paste0(getwd(),"/Data")

#####
df = read.csv(paste0(path,"/Train (2).csv")) 
id = df$Square_ID
target = df$target_2015

df = df %>% within(rm("target_2015"))
df2 = df %>% select(Square_ID,LC_Type1_mode,X,Y,elevation)
test = bind_cols(df2,df %>% select(contains("2019")))
df = bind_cols(df2,df %>% select(contains("2014")),
  df %>% select(contains("2015")))

rm(df2)
df$`precip 2014-12-28 - 2015-01-041`=NULL

####
colnames(df) = c(colnames(df[,1:5]),paste0("week_",1:ncol(df[,6:ncol(df)])))
colnames(test) = c(colnames(test[,1:5]),paste0("week_",1:ncol(test[,6:ncol(test)])))
df = bind_rows(df,test)

#### FEATURES
df$b = df$X*df$Y
df$ee = ifelse(df$elevation <250,0,1)
df$ee2 = df$elevation * df$X
df$ee3 = df$LC_Type1_mode * df$elevation


drop.cols = c("Square_ID",paste0("week_",1:4),paste0("week_",14:17))
df =df %>% select(-drop.cols)

train = df[1:16466,]
test = df[16467:32932,]
#### MY XGBOOST SCORE 0.768 
library(xgboost)
label = ifelse(target>0.5,1,0)
dtrain = xgb.DMatrix(as.matrix(train), label=label)
dtest = xgb.DMatrix(as.matrix(test[,colnames(train)]))

param = list(booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.01,
  colsample_bytree = 1,
  max_depth = 4,
  min_child_weight = 1,
  nthread = 8,
  gamma = 0,
  subsample = 0.8
)

set.seed(1235)
mod.xgb = xgb.train(data = dtrain,params = param,nrounds = 1000)

pred= predict(mod.xgb,dtest)
sub = cbind(id,pred)
write.csv(sub,file = "label.csv",row.names = F)

