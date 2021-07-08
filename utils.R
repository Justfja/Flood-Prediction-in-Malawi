

#### conect to SOAR package
fstoreconnect = function(subdir){
  oldLC = Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
  Sys.setenv(R_LOCAL_CACHe = subdir)
}
fstoreconnect("rstore")
tmp = Objects()

samp = fread(paste0(data.dir,"/SampleSubmission (1).csv"))
###
freq.encode = function(x ,xnew = x){
  if(is.factor(x) || is.character(x)){
    return(as.numeric(factor(xnew, levels = names(sort(table(x))))))
  }else{
    return(approxfun(density(x[!is.na(x)],n=length(x)/100))(xnew))
  }
}


group_alt = function(x){
  if(x<= 250){
    return(1)
  }else if(x>250 & x<= 500){
    return(2)
  }else if(x>500 & x<=1000){
    return(3)
  }else{
    return(4)
}
}

#### my f2 cnt
my.f2cnt = function(th2,vn1,vn2, filter = TRUE){
  data = data.frame(f1= th2[,vn1],f2=th2[,vn2], filter = filter)
  colnames(data) = c("f1","f2","filter")
  sum1 = sqldf::sqldf("select f1,f2, count(*) as cnt from data where filter=1 group by 1,2")
  tmp = sqldf::sqldf("select b.cnt from data a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp$cnt[is.na(tmp$cnt)] = 0
  return(tmp$cnt)
  
}



calc_entropy = function(df,group,subgrp,tgt_vn_prefix){
  sum1 = df[,.N,by =list(df[[group]],df[[subgrp]])]
  setnames(sum1,c(group,subgrp,"subgrpcnt"))
  sum2 = df[, .N, by = list(df[[group]])]
  setnames(sum2, c(group,"cnt"))
  sum3 = sum2 %>% left_join(sum1, by = c(group))
  sum3 = as.data.table(sum3)
  sum3[,entropy := -log(subgrpcnt *1/cnt)*subgrpcnt*1/cnt]
  sum3[is.na(entropy),entropy := 0]
  sum4 = sum3[,sum(entropy) ,by = list(sum3[[group]])]
  setnames(sum4, c(group,paste(tgt_vn_prefix,"entropy",sep = "_")))
  return(sum4)
}



# CUBIST training function
cubist.model <- function(intrain, label, intest){
  
  set.seed(1235)
  cub.mod <- cubist(x = as.matrix(intrain), y = label, committees = cub.param[1],
    neighbors = cub.param[2],
    control = cubistControl(unbiased = T, rules = cub.param[3], 
      sample = 0)) 
  
  pred <- predict(cub.mod, newdata = as.matrix(intest[,colnames(intrain)]))
  
  return(pred)
}


prepfun <- function(base, weather, building_metadata) {
  out <- base %>% 
    left_join(building_metadata, by = "building_id") %>% 
    left_join(weather, by = c("site_id", "timestamp")) %>% 
    mutate(timestamp2 = lubridate::as_datetime(timestamp),
      hour = lubridate::hour(timestamp),
      #day = mday(timestamp),
      #month = month(timestamp),
      weekday = as.integer(lubridate::wday(timestamp)),
      year_built = as.integer(year_built - 1900) ,
      square_feet = as.integer(log1p(square_feet)),
      floor_count = as.integer(floor_count),
      primary_use = as.integer(as.numeric(as.factor(primary_use))),
      air_temperature= as.integer(air_temperature),
      cloud_coverage = as.integer(cloud_coverage),
      dew_temperature = as.integer(dew_temperature),
      precip_depth_1_hr = as.integer(precip_depth_1_hr),
      wind_direction = NULL,
      wind_speed = NULL,
      sea_level_pressure = NULL,
      row_id = NULL,
      timestamp2 = NULL,
      # country = ifelse(site_id %in% a,1,
      #                  ifelse(site_id %in% b,2,
      #                     ifelse(site_id %in% c,3,4))),
      # offset = ifelse(site_id %in% d, 1,
      #                 ifelse(site_id %in% e,2,
      #                        ifelse(site_id %in% f,3,4))),
      #en = freq.encode(primary_use),
      #en2 = as.integer(freq.encode(square_feet)),
      #,
      #en4 = as.integer(freq.encode(meter)),
      #en5 = freq.encode(hour))  
      en3 = freq.encode(building_id)) %>% 
    add_count(building_id) %>%
    rename("building_id_cnt" = n)
}



#' count the number of peaks
#' @param x the input numeric vector of accoustic data
#' @param window the window size to use
#' @return a scalar numeric for the number of detected peaks
#' @export
number_peaks <- function(x, window){
  rollmax <- as.numeric(RollingMax(x, window))
  npeak <- sum(x > lead(rollmax, n = window) & x > lag(rollmax), na.rm = T)
  data.frame(npeak) %>% set_names(paste0("num_peaks_", window))
}

#' count the number of peaks with several windows
#' @param x the input numeric vector of accoustic data
#' @param windows the window sizes to use
#' @return a data frame with the number of peaks at different windows threshold
#' @export
peakFeatures <- function(x, windows = c(10)){
  map_dfc(windows, ~number_peaks(x, window=.x))
}


#' Computes spectrum features: the mean and variance at frequency bands from start to end, 1 to start, and end to 75000
#' @param x the input numeric accoustic data
#' @param start first frequency
#' @param end last frequency
#' @param by the step size between start and end
#' @return a data frame with spectrum features
#' @export
specFeatures <- function(x, start = 1000, end = 70000, by = 5000){
  s <- spectrum(x, plot = F)
  spec <-  s$spec
  bandStart <- seq(start, end, by)
  bandEnd <- bandStart + by - 1
  bandStart <- c(1, bandStart, end + by + 1)
  bandEnd <- c(start - 1, bandEnd, 5000)
  features <- map2_dfc(bandStart, bandEnd,
    ~ data.frame(meanBand = log(mean(spec[.x:.y])),
      varBand = log(var(spec[.x:.y]))))
  return(features)                           
}

#######
collectStats <- function(x, quant = c(0.05, 0.1,0.2,0.5,0.8, 0.95,0.9)){
  data.frame(t(quantile(x,quant))) %>%
    set_names(paste0("Q",quant))   %>%
    mutate(mean = mean(x), sd = sd(x), 
      min  = min(x), max = max(x), 
      IQR  = IQR(x), RMS = sqrt(mean(x^2)))
}


#' The feature extraction engine. It first denoise the data, then extract the features
#' @param x the input numeric accoustic data
#' @param time time to failure input to be used with training data, keep NULL with test data
#' @param train whether the input is training or testing data
#' @param wavelet the wavelet to be used for both denoising and decomposition
#' @return a data frame with extracted features
#' @export
extractFeatures <- function(x, time = NULL, train = T, wavelet = "l14" ){

  
  # collect some features inspired by time-series analysis
  # auto-correlation and partial autocorrelation in addition to entropy and stability
  features2 <- tsfeatures(as.ts(x), 
    features = c("entropy", "acf_features", "stability", "pacf_features"))
  
 
  
  # compute spectrum features on the signal
   features4 <- map_dfc(list(x),specFeatures)
  
  
  features5 <- map_dfc(list(x),peakFeatures)
  # join everything together
  #features <- data.frame(features2, features3, features4, features5)
  features <- data.frame(features2, features5)
  
 
  return(features)
}
