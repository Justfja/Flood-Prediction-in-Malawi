library(Boruta)
# Traffic_Boruta <- Traffic #For the Boruta package
# summary(Churn_Boruta)
# library(doParallel)
# registerDoParallel(cores = 4)
set.seed(123456)
s = Sys.time()
  Boruta_Out <- Boruta(label ~ . , data=tr, doTrace=2,maxRuns = 50)

Sys.time() - s


Farm_boruta_signif <- names(Boruta_Out$finalDecision[Boruta_Out$finalDecision %in% c("Confirmed","Tentative")])  # collect Confirmed and Tentative variables

# print(Traffic_boruta_signif)  # significant variables
# Farm_Imp <- as.data.frame(Farm_Boruta_Out$ImpHistory)
# Farm_Imp[is.na(Farm_Imp)] <- 0
# Farm_Imp <- sapply(Farm_Imp,FUN=mean)
# write.csv(Farm_Imp, file = "Farm_main_NDVI_NDRE.csv")
plot(Farm_Boruta_Out, cex.axis=0.6, las=2, xlab="", main="Variable Importance")   # plot variable importance
grid(ny = 100, lty = "dotted",lwd = 2)
save(Farm_Boruta_Out,Farm_boruta_signif, file = "Farm_Featue_Selection2.rda")