library(h2o)
h2o.init(nthreads = -1)
dr = as.h2o(dr.dat)
predictors = colnames(dr.dat)
hyperParamsAutoencoder = list( 
  hidden = list(c(11, 3, 11), c(10, 3, 10), c(9, 3, 9), c(8, 2, 8), 
                c(7, 2, 7), c(6, 5, 6), c(11, 3, 11), c(8, 5, 8), 
                c(9, 4, 9), c(8, 4, 8), c(7, 4, 7), c(6, 4, 6), 
                c(9, 3, 9), c(8, 3, 8), c(7, 3, 7), c(6, 3, 6),
                c(11, 4, 11), c(10, 4, 10), c(11, 8, 2, 8, 11)),
  activation = c("Tanh"))
#}

gridAutoencoder <- h2o.grid(
  x = predictors,
  autoencoder = TRUE,
  training_frame = dr,
  hyper_params = hyperParamsAutoencoder,
  search_criteria = list(strategy = "Cartesian"),
  algorithm = "deeplearning",
  grid_id = "grid_autoencoder", 
  reproducible = TRUE, 
  seed = 1,
  variable_importances = TRUE,
  categorical_encoding = "AUTO",
  score_interval = 10,
  epochs = 50,
  adaptive_rate = TRUE,
  standardize = TRUE,
  ignore_const_cols = FALSE)


sortedGridAutoencoder = h2o.getGrid("grid_autoencoder", 
                             sort_by = "mse", decreasing = FALSE)
tmpDf <- as.data.frame(sortedGridAutoencoder@summary_table)
knitr::kable(head(tmpDf[, -grep("model_ids", colnames(tmpDf))]), row.names = TRUE)

bestAutoencoder <- h2o.getModel(sortedGridAutoencoder@model_ids[[1]])

bestAutoencoderErr <- as.data.frame(h2o.anomaly(bestAutoencoder, 
                                                train, 
                                                per_feature = FALSE))


deepFeature2 <- h2o.deepfeatures(bestAutoencoder, dr, layer = 2)
