library(lightgbm)
library(caret)



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
               #boost_from_average = "false",
               #tree_learner = "serial",
               feature_fraction = 0.5,
               bagging_freq = 1,
               bagging_fraction = 0.8
               #lambda = 1,
               #alpha = 1
              # max_bin = 63
              # min_data_in_leaf = 90,
              # min_sum_hessian_in_leaf = 10
              #min_split_gain = 0.1,
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







devresult = rep(0,nrow(df_train))
predte = rep(0,nrow(df_test))
cvscore = c()
int.seed = c(500)


for (i in 1:length(int.seed)) {
  cat("model training",i,"\n")
  
  set.seed(int.seed[i])
  folds = createFolds(label, k = 5)
  
  param = list(objective = "binary",
    metric = "binary_logloss",
   # boost_from_average = "false",
    tree_learner = "serial",
    feature_fraction = 0.8,
    bagging_freq = 1,
    bagging_fraction = 0.8
    #is_unbalance = T
    #lambda = 1,
    #alpha = 1,
     #max_bin = 63,
    # min_data_in_leaf = 90,
  #  min_sum_hessian_in_leaf = 10
    #min_split_gain = 0.1,
  )
  
  for (this.round in 1:length(folds)) {
    cat("model training",i," ","fold ",this.round,"\n")
    valid = c(1:length(label2))[unlist(folds[this.round])]
    dev = c(1:length(label2))[unlist(folds[1:length(folds)!= this.round])]
    
    dtrain = lgb.Dataset(data = as.matrix(df_train[dev,]),label = label2[dev], free_raw_data = F)
    dvalid = lgb.Dataset(data = as.matrix(df_train[valid,]),label = label2[valid],free_raw_data= F)
    
    model = lgb.train(data = dtrain,
      params = param,
      nrounds = 1000,
      valids = list(val= dvalid),
      boosting_type = "gbdt",
      learning_rate = 0.01,
      max_depth = 15,
      num_leaves = 20,
      num_threads = 8,
      eval_freq = 500,
      seed = 1235,
      verbose = 1,
      early_stopping_rounds = 10
    )
    
    pred = predict(model,as.matrix(df_train[valid,]))
    devresult[valid] = pred
    pred_test = predict(model, as.matrix(df_test[,colnames(df_train)]))
    predte = predte + pred_test
    
    cat("model cv rmse score:", model$best_score,"\n")
    cvscore = c(cvscore, model$best_score)
    cat("model cv rmse mean score:",mean(cvscore), "\n")
  }
}





