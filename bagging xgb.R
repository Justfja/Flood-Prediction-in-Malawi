
# dir.create(paste0(path.dir,"/cvPred"))
 cv.dir = paste0(path.dir,"/cvPred")
# 

library(xgboost)

xgb_grid=expand.grid(eta = c(0.01,0.005),
                     colsample_bytree = c(0.8,0.9,1.0),
                     max_depth = c(4),
                     min_child_weight = c(1)
                     )

dtrain = xgb.DMatrix(as.matrix(df_train), label = label2)
dtest = xgb.DMatrix(as.matrix(df_test[,colnames(df_train)]))

watchlist =  list(train = dtrain)

xgbcv.res = data.frame(xgb_grid)
xgbcv.res$nrounds = 0
xgbcv.res$rmse = 0

### Bagging
bagpred_cv = matrix(0,nrow = nrow(df_train), ncol = nrow(xgb_grid))
bagpred_test = matrix(0,nrow = nrow(df_test), ncol = nrow(xgb_grid))

## model training
#library(xgboost)
for(i in 1:dim(xgb_grid)[1]){
  eta = xgb_grid[i,1]
  colsample_bytree = xgb_grid[i,2]
  max_depth= xgb_grid[i,3]
  min_child_weight = xgb_grid[i,4]
  
  param = list(booster = "gbtree",
               objective = "binary:logistic",
               eta = eta,
               colsample_bytree = colsample_bytree,
               max_depth = max_depth,
               min_child_weight = min_child_weight,
               nthread= 4,
               #gamma = 1.5,
               #lambda = 0,
               #base_score = 0,
               #alpha = 0,
               subsample= 0.8)
  
  set.seed(1235)
  fit_cv = xgb.cv(params = param,
                  data = dtrain,
                  watchlist = watchlist,
                  #nfold = 5,
                  folds = folds,
                  metrics = "logloss",
                  print_every_n = 500,
                  nrounds = 1000,
                  early_stopping_rounds = 10,
                  prediction = TRUE,
                  maximize = F)
  
  xgbcv.res[i,5] = fit_cv$best_iteration
  xgbcv.res[i,6] = fit_cv$evaluation_log[fit_cv$best_iteration]$test_logloss_mean
  
  bagpred_cv[,i] = fit_cv$pred
cvpred = data.frame(id = train.id,xgbcv_pred = bagpred_cv)
 colnames(cvpred) = c("Square_ID",paste0("xgb_mod",1:nrow(xgb_grid)))
# write.csv(cvpred,paste0(cv.dir,"/",Sys.Date(),"_xgb_bestdata_cv_mod_depth_4.csv"),row.names=F)


#
set.seed(1235+i)
mod.xgb = xgb.train(data = dtrain, params = param,
                    nrounds = round(xgbcv.res[i,5]),
                    print_every_n = 500)
bagpred_test[,i] = predict(mod.xgb,newdata = dtest)

# sub= data.frame(test.id,bagpred_test)
# colnames(sub) = c("Order.No",paste0("Time",1:nrow(xgb_grid)))
# write.csv(sub,file = paste0(subm.dir,"/",Sys.Date(),"_xgb_test_mod.csv"), row.names = F)

cat("Trained ", i ," of ", dim(xgb_grid)[1], "\n")
}