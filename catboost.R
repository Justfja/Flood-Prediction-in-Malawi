
library(catboost)

 # set.seed(501)
 # folds = createFolds(as.factor(label),k = 5)
devresult = rep(0,nrow(df_train))
p = rep(0, nrow(df_test))
dtest = catboost.load_pool(as.matrix(df_test[,feature1]))

### create a matrix for predictions
#pred_te <- rep(0, nrow(df_test))

for (i in 1) {
  
  print(paste0("model training on label ", i))
  
  #label = cv_label[,i]

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
  
  
  
 # set.seed(1235)
  model = catboost.train(learn_pool = dtrain,test_pool = dvalid,
                         params = params)
  
  predt = catboost.predict(model,
                      pool = catboost.load_pool(as.matrix(df_train[valid,feature3])),
                      prediction_type = "Probability")
  devresult[valid] = predt
  
  pred = catboost.predict(model, pool = dtest, 
                          prediction_type = "Probability")
  
  
  p = p + pred
}   
}


# fit_cat = catboost.cv(dtrain, params = params, fold_count = 5,
#                       partition_random_seed = 1235,
#                       early_stopping_rounds = 100)

library(CatEncoders)
label_enc = LabelEncoder.fit(label)
label3 = transform(label_enc,label) - 1


#set.seed(1235)
model = catboost.train(learn_pool = dtrain,test_pool = NULL,
  params = params)


y = raster::clamp(catboost.predict(model, pool=dtest),upper = 0.95,lower = 0)
