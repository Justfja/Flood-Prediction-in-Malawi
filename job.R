
library(xgboost)


dtrain = xgb.DMatrix(as.matrix(df_train[,feature2]), label=label2)
dtest = xgb.DMatrix(as.matrix(df_test[,feature2]))

param = list(booster = "gbtree",
             objective = "binary:logistic",#"reg:gamma",
             eval_metric = "logloss",
             eta = 0.01,
             colsample_bytree = 1,
             max_depth = 4,
             min_child_weight = 1,
            #scale_pos_weight = 0.9,
             #num_parallel_tree = 2,
             nthread = 8,
          #   base_score = mean(label),
             gamma = 0,
             subsample = 0.8
)

watchlist= list(train = dtrain)

set.seed(1235)
fit_cv = xgb.cv(params = param,
                data = dtrain,
                watchlist = watchlist,
                nrounds = 1000,
                stratified = T,
                nfold = 5,
                #folds = folds,
                print_every_n = 500,
                early_stopping_rounds = 10,
                prediction = F,
                maximize = FALSE)


set.seed(1235)
mod.xgb = xgb.train(data = dtrain,params = param,nrounds = 1000)
imp = as.data.frame(xgb.importance(feature_names = colnames(df_train),model = mod.xgb))


pred= predict(mod.xgb,dtest)
#pred = round(pred^2)

sub = cbind(test.id,pred)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/xgbsub.csv"),row.names = F)

sub = cbind(test.id,pred)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/label5.csv"),row.names = F)


sub = cbind(test.id,m)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/sub4.csv"),row.names = F)

#########


### Featire selection process
nfold = 5
fcrossv = function(param,nfold,vb=1){
  set.seed(1235)
  xgb.cv(param,
         dtrain,nrounds = 1000,
         nfold = nfold,
         maximize = F,
         early_stopping_rounds = 10,
         print_every_n = 1000,
         verbose = 1)
}

vimp = colnames(df_train)
crossv = fcrossv(param,nfold)
nrounds = crossv$best_ntreelimit

set.seed(1235)
mod0 = xgb.train(data = dtrain,
                 params = param,
                 nrounds = nrounds,
                 maximize =  F,
                 verbose = 1)

vmodel = colnames(dtrain)
imp = as.data.frame(xgb.importance(feature_names = vmodel,model = mod0))
imp$cumGain = cumsum(imp$Gain)
imp$n = 1:nrow(imp)
impgap = imp
gc()

s= Sys.time()
nfeat= length(impgap$Feature)
eval = 0.2
best = -1
eps = 0
featbag = impgap$Feature[1]
for (idx in 2:nfeat) {
  vadd = imp$Feature[idx]
  vimp = c(featbag,vadd)
  dtrain = xgb.DMatrix(as.matrix(df_train[,vimp]), label = label2)

  crossv = fcrossv(param,nfold,1)
  nrnds = crossv$best_ntreelimit

  tmpeval = crossv$evaluation_log$test_logloss_mean[nrnds]
  if((tmpeval+eps)<eval){eval = tmpeval; best = idx; featbag=vimp}

  print(c(idx,eval,best,length(featbag)));flush.console()
  rm(dtrain,crossv)
  gc()
}

Sys.time() 






### log loss trick
a = pred
for (i in 1:nrow(a)) {
  j = 1:9
  row = a[i,]
  max_val = max(row)
  preds = match(max_val, row)
  if(max_val > 0.9){
    a[i,preds] = 1
    a[i,j!= preds] = 0
  }
}

a = pred

for (i in 1:nrow(a)) {
  j = 1:9
  preds = 5
  if(a[i,preds] > 0.8){
    a[i,preds] = 1
    a[i,j != preds]=0
  }
}


library(h2o)

h2o.init(nthreads = -1)
tr = cbind(df_train,label2=as.factor(label2))

htrain = as.h2o(tr)
dtest = as.h2o(df_test)

aml = h2o.automl(x = seq(1,ncol(htrain)),
                 y = ncol(htrain),
                 training_frame = htrain,
                 project_name = "auto1",
                 nfolds = 5,seed = 1235,
                 max_runtime_secs = 1800,
                 balance_classes = TRUE,
                 stopping_metric = "logloss",
                 exclude_algos = c("GLM","DRF")
                 )
####
p = as.data.frame(h2o.predict(aml@leader,newdata = dtest))
pred = as.data.frame(h2o.predict(aml,newdata = dtest))


label3 = ifelse(label>0.5,"Yes","No")

############
grid = expand.grid(committees = c(20,30,40,50), neighbors = c(0))
grid = expand.grid(n.trees = seq(150,300,50), interaction.depth = c(3:5),
                   shrinkage = 0.1,n.minobsinnode = 10)
grid = expand.grid(size = c(50), decay = c(0.1,1),bag=F)
set.seed(12355)
mod = caret::train(x = da2[,-2],y = as.factor(da2$label), method = "gcvEarth",
                    metric = "logLoss",
                    trControl = trainControl(
                      method = "cv",
                      number = 5,
                      #classProbs = T,
                      verboseIter = T,
                      #summaryFunction = twoClassSummary
                    #savePredictions = "final"
                    ))#,tuneGrid = grid)


cub.param <- list(committees = 70, neighbors = 0, rules = 100, sample = 0)

a = cubist.model(df_train[,regression_feat],label = label, intest = df_test)


a = cubist.model(da2,label = label, intest = de3)


set.seed(1234)
rf = randomForest(x = df_train, y = as.factor(label2), ntree = 500,
                  importance = T)


start.time = Sys.time()
set.seed(1235)
model = train(x = as.matrix(da[,-3]), y = as.factor(label2), method ="glmnet",
  metric = "logLoss",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = F,
    #classProbs = T,
    #summaryFunction =twoClassSummary,
    savePredictions = "final"
  ))

total.time = Sys.time() - start.time
total.time 