# ########## Forecasting AR(2)
# #libraries used:

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("ForecastSupportFunctions.R")
lisOfUsedPackages <- c("curl","tseries", "rstudioapi", "openxlsx", "GMDH","neuralnet",  "caret","mda","earth", "ggplot2", "randomForest", "bibtex","rpart","xtable", "zoo", "scales", "forecast")
CheckInstalledPackages(lisOfUsedPackages)

# uploading the data
fileToRead <- 'ALL_DATA.xlsx'
mainData <- ReadDataFromExcel(fileToRead)
mainDataUntransformed<-mainData[-1:-4,]
maxHorizon <<- 8

#TRANSFORMING THE WHOLE DATASET
transformingCodes <- data.frame(t(c(5, 5, 4, 5, 5, 5, 5, 5, 5, 5, 2, 5, 5, 1, 1, 1, 1)))
transformedData <- TransProcedure(transformingCodes, mainData)

#PLOTTING CONNECTION BETWEEN PRESENT VALUE OF CPI AND OTHER VARIABLES
plotConnection1 <- FALSE
  if (plotConnection1) {
PlotConnectionCPICurrent(mainData)
PlotConnectionCPICurrent(transformedData)
}

#Creating TS for AR(2) and GMDH
stationaryFullSeries<-CreateStationarySeries(mainData)

#creating a list of datasets for Pseudo-out-of-sample
isTransformed <- TRUE
listOfLaggedTransformedData<-createListOFLaggedDatasets(transformedData, isTransformed)
listOfLaggedData<- createListOFLaggedDatasets(mainDataUntransformed)

#Plotting connection between present value of CPI and other variables in previous periods+prevoius CPI
plotConnection <- FALSE
if (plotConnection) {
  PlotConnectionCPIPrevious(listOfLaggedTransformedData)
}

listOfModels<-list()
#AR(2) using the functions of R
listOfModels[[1]]<- AR_2(stationaryFullSeries)  
names(listOfModels)[length(listOfModels)]<-"AR(2), stationary"
#AR(2) for untransformed data:
listOfModels[[2]]<-AR_2(mainData[,1])
names(listOfModels)[length(listOfModels)]<-"AR(2), raw data"

#using GMDH, written by Osman Dag, Ceylan Yozgatligil, works only with univariate timeseries, 
#they provide control of the number of lugged inputs which may be used for forecasting
listOfModels[[3]]<-RGMDHApplying(stationaryFullSeries)
names(listOfModels)[length(listOfModels)]<-"RGMDH,3 lagged CPI, stationary"
listOfModels[[4]]<-GMDHApplying(stationaryFullSeries)
names(listOfModels)[length(listOfModels)]<-"GMDH, 4 lagged CPI, stationary"
listOfModels[[5]]<-RGMDHApplying(fullSeries)
names(listOfModels)[length(listOfModels)]<-"RGMDH,3 lagged CPI, raw data"
listOfModels[[6]]<-GMDHApplying(fullSeries)
names(listOfModels)[length(listOfModels)]<-"GMDH, 4 lagged CPI, raw data"

#Using Multivariate Adaptive Regression Splines  by Trevor Hastie and Robert Tibshirani
#On transformed data:
listOfModels[[7]]<- MARSApplying(listOfLaggedTransformedData)
names(listOfModels)[length(listOfModels)]<-"MARS, 1 lag of all, stationary"
listOfModels[[8]]<- MARSApplying(listOfLaggedData)
names(listOfModels)[length(listOfModels)]<-"MARS, 1 lag of all, raw data"
#Using the EARTH, the result models are almost the same but earth have the funciton of describtion of the fit
# helps to figure out which variables have predictive power
#using with transformed data(just to show tha we should not use Multivariate adaptive regression splines with transformed to stationarity data)
listOfModels[[9]]<- EARTHApplying(listOfLaggedTransformedData)
names(listOfModels)[length(listOfModels)]<-"EARTH, 1 lag of all, stationary"
#using the same with untransformed data
listOfModels[[10]]<- EARTHApplying(listOfLaggedData)
names(listOfModels)[length(listOfModels)]<-"EARTH, 1 lag of all, raw data"
#Ilustrations: plots for CPI and other presictors in time(8 steps at one screen)
for ( t in (1:(ncol(mainData)))){
    PlotDepeendency(mainData[,t],mainData$CPI, colnames(mainData)[t], "CPI",plotToFile = FALSE) 
}
for ( t in (1:(ncol(transformedData)))){
  PlotDepeendency(transformedData[,t],transformedData$CPI, colnames(transformedData)[t], "CPI",plotToFile =TRUE) 
}

laggedDataRich<- CreateDatasetWithLags(mainDataUntransformed)
laggedDataRichTransformed<-CreateDatasetWithLags(transformedData)

listOfModels[[11]]<-MARSApplying(laggedDataRich)
names(listOfModels)[length(listOfModels)]<-"MARS, lagged values, raw data"

listOfModels[[12]]<-MARSApplying(laggedDataRichTransformed)
names(listOfModels)[length(listOfModels)]<-"MARS, lagged values, stationary"

listOfModels[[13]]<-EARTHApplying(laggedDataRich)
names(listOfModels)[length(listOfModels)]<-"EARTH, lagged values, raw data"

listOfModels[[14]]<-EARTHApplying(laggedDataRichTransformed)
names(listOfModels)[length(listOfModels)]<-"EARTH, lagged values, stationary"

#for the data of 8 steps forecast, with 4 steps laggs, processing time:26.58494 mins,  29.9085 mins
  listOfModels[[15]]<-MyGmdhApplying(laggedDataRich)
  names(listOfModels)[length(listOfModels)]<-"GMDH, lagged values, raw data"

listOfModels[[16]]<-MyGmdhApplying(laggedDataRichTransformed)
names(listOfModels)[length(listOfModels)]<-"GMDH, lagged values, stationary"

#applying CART:
listOfModels[[17]]<-CARTApplying(listOfLaggedData)
names(listOfModels)[length(listOfModels)]<-"CART, 1 lag of all, raw data"

listOfModels[[18]]<-CARTApplying(laggedDataRich)
names(listOfModels)[length(listOfModels)]<-"CART, lagged values, raw data"

listOfModels[[19]]<-CARTApplying(listOfLaggedTransformedData)
names(listOfModels)[length(listOfModels)]<-"CART, 1 lag of all, stationary"

listOfModels[[20]]<-  CARTApplying(laggedDataRichTransformed)
names(listOfModels)[length(listOfModels)]<-"CART, lagged values, stationary"

#Applying random forest
listOfModels[[21]]<-RandomForestApplying(listOfLaggedTransformedData,3,9)
names(listOfModels)[length(listOfModels)]<-"Random forest, 1 lag of all, stationary"
  
listOfModels[[22]]<-RandomForestApplying(listOfLaggedData,3,9)
names(listOfModels)[length(listOfModels)]<-"Random forest, 1 lag of all, raw data"

listOfModels[[23]]<-RandomForestApplying(laggedDataRichTransformed,3,21)
names(listOfModels)[length(listOfModels)]<-"Random forest, lagged values, stationary"

listOfModels[[24]]<-RandomForestApplying(laggedDataRich,3,18)
names(listOfModels)[length(listOfModels)]<-"Random forest, lagged values, raw data"

set.seed(10)
listOfModels[[25]]<-MyGmdhApplying(listOfLaggedData)
names(listOfModels)[length(listOfModels)]<-"GMDH, 1 lag of all, raw data"
set.seed(10)
listOfModels[[26]]<-MyGmdhApplying(listOfLaggedTransformedData)
names(listOfModels)[length(listOfModels)]<-"GMDH, 1 lag of all, stationary"

#Creating MSFE table
MSE<-FormMSE(listOfModels)
bestmodelsForRaw<-FindBest(MSE,TRUE)
bestmodelsForStationary<-FindBest(MSE,FALSE)

#Plotting results
#for transformed:
PlottingBest(realSeries =stationaryFullSeries,nameOffile = "ForecastStationary",
             vectorNumBestModels = bestmodelsForStationary,listOfModels = listOfModels,isTransformed = TRUE)
#for untransformed:
PlottingBest(realSeries = fullSeries,nameOffile = "ForecastReal",
             vectorNumBestModels = bestmodelsForRaw,isTransformed = FALSE,listOfModels )
#Creating tables with most frequently used predictors
vectorWithEarth<-which(grepl("EARTH",rownames(MSE$All)))
data_favors<-FindPredictors(vectorWithEarth)
xtable(data_favors,caption = "Most frequently used predictors by EARTH")

vectorWithGMDH<-which(grepl("GMDH",rownames(MSE$All)) & ! grepl("CPI",rownames(MSE$All)))
data_favors_gmdh<-FindPredictors(vectorWithGMDH)
xtable(data_favors_gmdh,caption = "Most frequently used predictors by GMDH")

vectorWithRandomForest<-which(grepl("Random forest",rownames(MSE$All)))
data_favors_RF<-FindPredictors(vectorWithRandomForest)
xtable(data_favors_RF,caption = "Most frequently used predictors by Random Forest")

#tables for describing datasets with which we make computations 
xtable(rbind(head(listOfLaggedData[[2]])[1:3,-(4:14)],tail(listOfLaggedData[[2]])[-(1:3),-(4:14)]))
xtable(rbind(head(listOfLaggedData[[2]])[1:3,-(4:15)],tail(listOfLaggedData[[2]])[-(1:3),-(4:15)]))
xtable(rbind(head(listOfLaggedTransformedData[[2]])[1:3,-(4:15)],tail(listOfLaggedTransformedData[[2]])[-(1:3),-(4:15)]))
xtable(rbind(head(laggedDataRich[[2]])[1:3,c(1,2,17,18,34,35,69)],tail(laggedDataRich[[2]])[-(1:3),c(1,2,17,18,34,35,69)]))
xtable(rbind(head(laggedDataRich[[2]])[1:3,c(1,2,18,19,35,36,69)],tail(laggedDataRich[[2]])[-(1:3),c(1,2,18,19,35,36,69)]))
xtable(rbind(head(laggedDataRichTransformed[[2]])[1:3,c(1,2,18,19,35,36,69)],tail(laggedDataRichTransformed[[2]])[-(1:3),c(1,2,18,19,35,36,69)]))

#creating tables containing DM test
DieboldMarianoTestStationary<-ApplyDMstatistics(listOfModels)
DieboldMarianoTestRaw<-ApplyDMstatistics(listOfModels,stationary = FALSE)
stationaryRes<-cbind(stars1(round(MSE$RelStationary,3),DieboldMarianoTestStationary), round(apply(X = MSE$RelStationary[-1,1:5],MARGIN =1,FUN = sum),2))
rawRes<-cbind(stars1(round(MSE$RelRaw,3),DieboldMarianoTestRaw), round(apply(X = MSE$RelRaw[-1,1:5],MARGIN =1,FUN = sum),2))
#compose this line:
xtable(t(table(round(MSE$Raw[1,],3))))
xtable(t(table(round(MSE$Stationary[1,],3))))
#with:
xtable(rawRes)
xtable(stationaryRes)