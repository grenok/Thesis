#Functions separeted from the main script for improving readability
source("gmdh.R")
firstYearOfData <- 1947
lastYearBasicModel <- 1974
quartelyFrequency <<- 4

#a function helping to define date using iteration number for poos
CalculateEndYearAndQ <- function(index){
  #Defining from which year we start poos
  q <- index%%4
  if (q==0){
    q<-quartelyFrequency
    year <- lastYearBasicModel+index%/%quartelyFrequency
  }
  else{year <- lastYearBasicModel+index%/%quartelyFrequency+1}
  return(c(year, q))
}

DataTransform <- function(data, code){
  epsilon<-1.0e-040
  if(code == 1){
    return(data)
  }
  else if (code == 2){
    return(diff(data, lag =1, differences = 1))
  }
  else if (code == 3){
    return(diff(data, lag =1, differences = 2))
  }
  else if (code == 4){
    if(min(data)< epsilon){ return (NaN) }
    else{
      return(log(data))
    }
  }
  else if (code == 5){
    if(min(data)< epsilon){ return (NaN) }
    else{
      return(diff(log(data),lag =1, differences = 1))
    }
  }
  else{ print("Error in transformation code")
    return(NaN)  }
}

CalculateNo<- function(year){
    if (year[2]%%4==0){
    return(4*(year[1]-1974))
  }
  else{
  return(4*(year[1]-1974-1)+year[2]%%4)}
}

TransProcedure<- function(transformingCodes,mainData){
  transformedData <- data.frame(matrix(0, nrow(mainData) - 4, NCOL(mainData)))
  colnames(transformedData) <- colnames(mainData)
  rownames(transformedData) <- rownames(mainData[-(1:4), ])
  for (i in 1:ncol(transformingCodes)) {
    code <- transformingCodes[1, i]
    if (code == 5) {
      #Annualizing the growth rate
      transformedData[2:length(mainData[-(1:4), i]), i] <-
        400 * DataTransform(mainData[-(1:4), i], code)
    } else if (code == 4) {
      transformedData[, i] <- 4 * DataTransform(mainData[-(1:4), i], code)
    }
    else if (code == 2) {
      transformedData[2:length(mainData[-(1:4), i]), i] <-
        4 * DataTransform(mainData[-(1:4), i], code)
    }
    else{
      transformedData[, i] <- DataTransform(mainData[-(1:4), i], code)
    }
  }
  transformedData<-data.frame(transformedData[-1,])
  return(transformedData)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


CheckInstalledPackages <- function(lisOfUsedPackages) {
  #loading the packages
  for(item in lisOfUsedPackages)
  {
    if(item %in% rownames(installed.packages()) == FALSE) {install.packages(item, dependencies = TRUE)}
    print(item)
    library(package = item, character.only = TRUE)
  }
}

ReadDataFromExcel <- function(fileToRead) {
  listOfAllfiles<-list.files(path = "../", full.names = TRUE, recursive = TRUE )
  dataPath <- listOfAllfiles[grep(paste0("/",fileToRead), listOfAllfiles)[1]]
  wBook<-loadWorkbook(dataPath)
  numOfLastDataAvailiableColumn <- 165
  mainData<- data.frame()
  for(item in wBook$`.->sheet_names`[-length(wBook$`.->sheet_names`)]){
    dataToProcess <- as.numeric(unlist(read.xlsx(wBook , sheet = item,  rowNames = TRUE, colNames = FALSE, startRow = 2, cols = c(1,numOfLastDataAvailiableColumn) )))
    if(length(mainData) > 0){
      mainData<-data.frame(mainData,dataToProcess)
    }else{
      mainData<- data.frame(dataToProcess)
      rownames(mainData)<- rownames(read.xlsx(wBook , sheet = item,  rowNames = TRUE, colNames = FALSE, startRow = 2, cols = c(1,numOfLastDataAvailiableColumn) ))
    }
    colnames(mainData)[length(mainData)]<-item
  }
  return(mainData)
}

PlotConnectionCPICurrent<- function(mainData){
  windows()
  plots<-list()
  for(item in 2:length(mainData)){
    plots[[item-1]] <-   ggplot(mainData, aes_string(colnames(mainData)[item], colnames(mainData)[1])) +
      geom_point(alpha=0.5) +geom_smooth()+  xlab(colnames(mainData)[item])
  }
  multiplot(plotlist = plots, cols = 4)
}
  
PlotConnectionCPIPrevious<- function(stepTransformedData){
  for (i in 1:length(stepTransformedData)) {
    windows(title = paste("step ", i))
    plots <- list()
    for (item in 1:(ncol(stepTransformedData[[i]]) - 1)) {
      plots[[item]] <-
        ggplot(stepTransformedData[[i]], aes_string(
          colnames(stepTransformedData[[i]])[item], colnames(stepTransformedData[[i]])[ncol(stepTransformedData[[i]])])) +
        geom_point() + geom_smooth() +  xlab(colnames(stepTransformedData[[i]])[item])
    }
    multiplot(plotlist = plots, cols = 4)
  }
}



AR_2<- function(stationaryFullSeries){
  #converting data to standard time series in R for using functions building AR(2), stationarity transformation
  #fullSeries<-ts(mainData$CPI, start = c(firstYearOfData,1),  frequency = quartelyFrequency)
  #stationaryFullSeries<-400*DataTransform(fullSeries, code = transformingCodes[1,1])
  fc_values<-list()
  
  MSE <- matrix(0,1,maxHorizon)
      if(!is.ts(stationaryFullSeries)){stationaryFullSeries<-ts(stationaryFullSeries,start = c(firstYearOfData,1), frequency = quartelyFrequency)}
  numForecast<- length(window(stationaryFullSeries, start = CalculateEndYearAndQ(1), frequency = quartelyFrequency))
  
  oldWarnings <- getOption("warn")
  options(warn = -1)
  for (k in 1:maxHorizon){
    fc<-c()
    for (i in 0:(numForecast-k)){
      ARmodel <- ar.ols( ts(stationaryFullSeries, start = c(firstYearOfData,2), end = CalculateEndYearAndQ(i), frequency = quartelyFrequency), aic= FALSE, order.max=2, demean=FALSE,intercept=TRUE)
      prediction <- predict(ARmodel,n.ahead=k)
      fc<-c(fc,prediction$pred[k])
      realWindowForComparing <- window(stationaryFullSeries, start = CalculateEndYearAndQ(i+1), end= CalculateEndYearAndQ(i+k))
      MSE[1,k] = MSE[1,k]+(prediction$pred[k] - realWindowForComparing[k])^2
    }
    fc_values[[k]]<-ts(fc,start = CalculateEndYearAndQ(k),  frequency = quartelyFrequency)
    MSE[1,k]<-MSE[1,k]/(numForecast-k)
  }
  options(warn = oldWarnings)
  MSE
  return(list("MSE"=MSE, "fc"=fc_values))
}

RGMDHApplying<- function(data){
  numForecast <- length(window(data, start = CalculateEndYearAndQ(1), frequency = quartelyFrequency))
  MSE<-matrix(0,1,maxHorizon)
  oldWarnings <- getOption("warn")
  options(warn = -1)
  modelNumber<- 1
  fc_values<-list()
  for (k in 1:5){
    fc<-c()
    for (i in (0:(numForecast-k))){
      prediction <- fcast(ts(data, start = 1, end = 111+CalculateNo(CalculateEndYearAndQ(i)), frequency = 1),tf= "all", method = "RGMDH",input =3, f.number = k)
      prediction<- ts(prediction$mean, start  = CalculateEndYearAndQ(i+1), end = CalculateEndYearAndQ(i+k), frequency = quartelyFrequency)
      fc<-c(fc,prediction[k])
      realWindowForComparing <- window(data, start = CalculateEndYearAndQ(i+1), end= CalculateEndYearAndQ(i+k))
      MSE[modelNumber,k] = MSE[modelNumber,k]+(prediction[k] - realWindowForComparing[k])^2
    }
    fc_values[[k]]<-ts(fc,start = CalculateEndYearAndQ(k),  frequency = quartelyFrequency)
    MSE[modelNumber,k]<-MSE[modelNumber,k]/(numForecast-k)
  }
  return(list(MSE,fc_values))
}

GMDHApplying<-function(data){
  numForecast <- length(window(data, start = CalculateEndYearAndQ(1), frequency = quartelyFrequency))
  MSE<-matrix(0,1,maxHorizon)
  oldWarnings <- getOption("warn")
  options(warn = -1)
  modelNumber<- 1
  fc_values_2<-list()
  for (k in 1:5){
    fc<-c()
    for (i in (0:(numForecast-k))){
      prediction <- fcast(ts(data, start = 1, end = 111+CalculateNo(CalculateEndYearAndQ(i)), frequency = 1),tf= "all", method = "GMDH",input =4, f.number = k, layer = 1)
      prediction<- ts(prediction$mean, start  = CalculateEndYearAndQ(i+1), end = CalculateEndYearAndQ(i+k), frequency = quartelyFrequency)
      print(prediction[k])
      fc<-c(fc,prediction[k])
      realWindowForComparing <- window(data, start = CalculateEndYearAndQ(i+1), end= CalculateEndYearAndQ(i+k))
      MSE[modelNumber,k] = MSE[modelNumber,k]+(prediction[k] - realWindowForComparing[k])^2
    }
    fc_values_2[[k]]<-ts(fc,start = CalculateEndYearAndQ(k),  frequency = quartelyFrequency)
    MSE[modelNumber,k]<-MSE[modelNumber,k]/(numForecast-k)
  }
  modelNumber<- modelNumber+1
  return(list(MSE, fc_values_2))
}

CreateStationarySeries <- function(mainData){
  fullSeries<<-ts(mainData$CPI, start = c(firstYearOfData,1),  frequency = quartelyFrequency)
  return(400*DataTransform(fullSeries, code = transformingCodes[1,1]))
}

createListOFLaggedDatasets<- function(Data, istransformed = FALSE){
  listOfLaggedData<-list()
  for(i in 1:maxHorizon){
    listOfLaggedData[[i]]<-data.frame(cbind(Data[-c((nrow(Data)-i+1):nrow(Data)), ],Data$CPI[-c(1:i)]))
    colnames( listOfLaggedData[[i]])[ncol( listOfLaggedData[[i]])]<-"CPI_f"
    rownames(listOfLaggedData[[i]])<- rownames(Data)[-c(1:i)]
  }
  initSampleLength<<- length(window(fullSeries, start = c(firstYearOfData,1),end =CalculateEndYearAndQ(0), frequency = quartelyFrequency))
  for ( i in length(listOfLaggedData):1){
    listOfLaggedData[[i+1]]<-listOfLaggedData[[i]]
  }
  if(istransformed){listOfLaggedData[[1]]<-initSampleLength-6}
  else{
    listOfLaggedData[[1]]<-initSampleLength-5
  }
  return(listOfLaggedData)
}

MARSApplying<- function(listOfData ){
  MSE<- matrix(0,1,maxHorizon)
  fc_values<-list()
  for(j in 2:length(listOfData)){
    fc<-c()
    for( i in 0:(nrow(listOfData[[j]])-listOfData[[1]]-1)){
      #Model is constructed so that it predicts only the step which we are interested in and only
      fit<- mars(listOfData[[j]][1:listOfData[[1]]+i,-ncol(listOfData[[j]])],listOfData[[j]][1:listOfData[[1]]+i,ncol(listOfData[[j]])])
      pred<- predict(fit,listOfData[[j]][listOfData[[1]]+i+1,-ncol(listOfData[[j]])])
      fc<-c(fc,pred)
      MSE[1,j-1]<-MSE[1,j-1]+(listOfData[[j]][listOfData[[1]]+i+1,ncol(listOfData[[j]])]-pred)^2
    }
    fc_values[[j-1]]<-ts(fc,start = CalculateEndYearAndQ(j-1),  frequency = quartelyFrequency)
    MSE[1,j-1]<-MSE[1,j-1]/(nrow(listOfData[[j]])-listOfData[[1]])
  }
  return(list(MSE,fc_values))
}

EARTHApplying<- function(listOfData){
  setOfVars<-list()
  MSE<- matrix(0,1,maxHorizon)
  result<- list()
  #Empirically found:
  modelTermsBeforePruning<-3
  fc_values<-list()
  for( j in 2:length(listOfData)){
    fc<-c()
   for( i in 0:(nrow(listOfData[[j]])-listOfData[[1]]-1)){
          fit <- earth(CPI_f~., listOfData[[j]][1:listOfData[[1]]+i,],nk=modelTermsBeforePruning) #The training set is growing by 1 observation
          pred<-predict(fit,listOfData[[j]][listOfData[[1]]+i+1,])
          fc<-c(fc, pred)
          MSE[1,j-1]<-MSE[1,j-1]+(pred-listOfData[[j]][listOfData[[1]]+i+1,ncol(listOfData[[j]])])^2
          t<-evimp(fit)
          for(item in dimnames(t)[[1]]){
            if(grepl("unused", item)){ next}
            if(length(setOfVars)>0){
              if(!(item %in% rownames(setOfVars))){
                setOfVars<-rbind(setOfVars,1)
                rownames(setOfVars)[nrow(setOfVars)]<-item
              }else{setOfVars[item,]<-setOfVars[item,]+1}
            }else{setOfVars<-data.frame(1)
            rownames(setOfVars)[nrow(setOfVars)]<-item}
          }
   }
    fc_values[[j-1]]<-ts(fc,start = CalculateEndYearAndQ(j-1),  frequency = quartelyFrequency)
        MSE[1,j-1]<-MSE[1,j-1]/(nrow(listOfData[[j]])-listOfData[[1]]-1)
       
      }
      rownames(MSE)[nrow(MSE)]<-paste0("Earth, order=",modelTermsBeforePruning)
      
    print("table of frequencies:")
    print(setOfVars)
    return(list("MSE"=MSE, "FC"=fc_values,"Frequencies of usage variables"=setOfVars))
}

PlotDepeendency<-function(xSeries, Yseries,nameX, nameY, Transformed=FALSE, plotToFile=TRUE){
dataForPlotting<-data.frame(Yseries)
for (i in 1:(maxHorizon)){
  dataForPlotting<-cbind.data.frame(dataForPlotting[-nrow(dataForPlotting),],dataForPlotting[-1,ncol(dataForPlotting)])
  colnames(dataForPlotting)[i+1]<-paste0(nameY,i)
}
colnames(dataForPlotting)[1]<-nameX
dataForPlotting[,1]<-xSeries[1:nrow(dataForPlotting)]

if (!plotToFile){windows()}
else{
  if(Transformed){png(paste0("transformed_data_",nameX,".png"), width=1920, height=1118, res=120)}
  else{png(paste0(nameX,"-",nameY,".png"), width=1920, height=1118, res=120)}
}
plots<-list()
for(item in 1:maxHorizon){
  plots[[item]] <-   ggplot(dataForPlotting, aes_string(x=colnames(dataForPlotting)[1], y=colnames(dataForPlotting)[item+1])) + geom_point() +geom_smooth() #+  xlab("CPI(t)")+ylab(paste0("CPI(t+",item,")"))
}

multiplot(plotlist = plots, cols = 4)
dev.off()
}
MyGmdhApplying<- function(listOfData ){
  MSE<- matrix(0,1,maxHorizon)
  Predictors<-data.frame()
  Pairs<-data.frame()
  fc_values<-list()
  for(j in 2:length(listOfData)){
    fc<-c()
    for( i in 0:(nrow(listOfData[[j]])-listOfData[[1]]-1)){
      #Model is constructed so that it predicts only the step which we are interested in and only
      Split<-sample.split(listOfData[[j]][1:listOfData[[1]]+i,ncol(listOfData[[j]])],0.7)
      net<-GMDH(listOfData[[j]][1:listOfData[[1]]+i,-ncol(listOfData[[j]])],listOfData[[j]][1:listOfData[[1]]+i,ncol(listOfData[[j]])],Split)
        if(j>5) { Predictors<-CountingPredictiveVariables(net,listOfData[[j]],listOfNames = Predictors)}
      Pairs<-CountingPredictivPairs(net, listOfData[[j]],listOfNames = Pairs)
      pred<-ResultOfnet(net,listOfData[[j]][listOfData[[1]]+i+1,-ncol(listOfData[[j]])])
      fc<-c(fc, pred)
      MSE[1,j-1]<-MSE[1,j-1]+(listOfData[[j]][listOfData[[1]]+i+1,ncol(listOfData[[j]])]-pred)^2
     # MSE[1,j-1]<-MSE[1,j-1]+(min(listOfData[[j]][listOfData[[1]]+i+1,ncol(listOfData[[j]])]-pred))^2
    }
    fc_values[[j-1]]<-ts(fc,start = CalculateEndYearAndQ(j-1),  frequency = quartelyFrequency)
    MSE[1,j-1]<-MSE[1,j-1]/(nrow(listOfData[[j]])-listOfData[[1]])
  }
  return(list("MSE"=MSE,"FC"=fc_values,"Predictors"=Predictors,"Net"=net, "Pairs"= Pairs))
}

CreateDatasetWithLags<- function(mainDataUntransformed, maxNumberOfLags =4, numberOfForecast=8){
  #creating the table with lags of predictors and step forecast value
  laggedDataRich<-list()
  #set of lagged predictors:
  numberOfvars<-ncol(mainDataUntransformed)
  startingData<-mainDataUntransformed
  colnames(startingData)<-paste0(colnames(startingData),"_t-",maxNumberOfLags)
  for( j in 1:(maxNumberOfLags-1)){
    startingData<-cbind(startingData[-nrow(startingData),],mainDataUntransformed[-(1:j),])
    colnames(startingData)[(ncol(startingData)-numberOfvars+1):ncol(startingData)]<-paste0(colnames(mainDataUntransformed),"_t-",maxNumberOfLags-j)
  }
  #rownames(startingData)<-rownames(mainDataUntransformed)[-1:-3]
  #laggedDataRich[[1]]<-initSampleLength-5
  laggedDataRich[[1]]<-104
  for(i in 2:(numberOfForecast+1)){
    laggedDataRich[[i]]<-cbind(startingData[-(nrow(startingData):(nrow(startingData)-i+2)),],mainDataUntransformed[-(1:(j+i-1)),1])
    colnames(laggedDataRich[[i]])[ncol(laggedDataRich[[i]])]<-"CPI_f"
    rownames(laggedDataRich[[i]])<-rownames(mainDataUntransformed)[5:(length(rownames(mainDataUntransformed))-i+2)]
  }
  return(laggedDataRich)
}

RandomForestApplying<-function(listOfData,k,l){
  MSE<- matrix(0,1,maxHorizon)
  fc_values<-list
  for(j in 2:length(listOfData)){
    fc<-c()
    for( i in 0:(nrow(listOfData[[j]])-listOfData[[1]]-1)){
      #Model is constructed so that it predicts only the step which we are interested in and only
      forest<- randomForest(listOfData[[j]][1:listOfData[[1]]+i,-ncol(listOfData[[j]])],listOfData[[j]][1:listOfData[[1]]+i,ncol(listOfData[[j]])], nodesize = k,mtry = l)
      pred<- predict(forest,listOfData[[j]][listOfData[[1]]+i+1,-ncol(listOfData[[j]])])
      fc<-c(fc, pred)
      MSE[1,j-1]<-MSE[1,j-1]+(listOfData[[j]][listOfData[[1]]+i+1,ncol(listOfData[[j]])]-pred)^2
    }
    fc_values[[j-1]]<-ts(fc,start = CalculateEndYearAndQ(j-1),  frequency = quartelyFrequency)
    MSE[1,j-1]<-MSE[1,j-1]/(nrow(listOfData[[j]])-listOfData[[1]])
  }
  return(list(MSE,fc_values))
}

CARTApplying<-function(listOfData){
  MSE<- matrix(0,1,maxHorizon)
  fc_values<-list()
  for(j in 2:length(listOfData)){
    fc<-c()
    for( i in 0:(nrow(listOfData[[j]])-listOfData[[1]]-1)){
      #Model is constructed so that it predicts only the step which we are interested in and only
      #cart<- rpart(CPI_f~., listOfData[[j]][1:listOfData[[1]]+i,], method= "anova",control = rpart.control(minsplit=2, cp=0.001,xval =20,minbucket =1,maxcompete = 15))
      w<-ncol(listOfData[[j]])
      cart<- rpart(CPI_f~., listOfData[[j]][1:listOfData[[1]]+i,], method= "anova",control = rpart.control(minsplit=5, xval =20,minbucket =3))
      pred<- predict(cart,listOfData[[j]][listOfData[[1]]+i+1,])
      fc<-c(fc,pred)
      MSE[1,j-1]<-MSE[1,j-1]+(listOfData[[j]][listOfData[[1]]+i+1,ncol(listOfData[[j]])]-pred)^2
    }
    fc_values[[j-1]]<-ts(fc,start = CalculateEndYearAndQ(j-1),  frequency = quartelyFrequency)
    MSE[1,j-1]<-MSE[1,j-1]/(nrow(listOfData[[j]])-listOfData[[1]])
  }
  return(list(MSE,fc_values))
}

RandomForestApplying<-function(listOfData,k,t){
  MSE<- matrix(0,1,maxHorizon)
  fc_values<-list()
  varPredictors<-matrix(0,nrow = ncol(listOfData[[2]])-1,ncol = 1)
  rownames(varPredictors)<-colnames(listOfData[[2]])[-ncol(listOfData[[2]])]
  for(j in 2:length(listOfData)){
    fc<-c()
    for( i in 0:(nrow(listOfData[[j]])-listOfData[[1]]-1)){
      #Model is constructed so that it predicts only the step which we are interested in and only
      forest<- randomForest(mtry =t,listOfData[[j]][1:listOfData[[1]]+i,-ncol(listOfData[[j]])],listOfData[[j]][1:listOfData[[1]]+i,ncol(listOfData[[j]])], nodesize = k)
      varPredictors<-varPredictors+varUsed(forest)
      pred<- predict(forest,listOfData[[j]][listOfData[[1]]+i+1,-ncol(listOfData[[j]])])
      fc<-c(fc,pred)
      MSE[1,j-1]<-MSE[1,j-1]+(listOfData[[j]][listOfData[[1]]+i+1,ncol(listOfData[[j]])]-pred)^2
    }
    fc_values[[j-1]]<-ts(fc,start = CalculateEndYearAndQ(j-1),  frequency = quartelyFrequency)
      MSE[1,j-1]<-MSE[1,j-1]/(nrow(listOfData[[j]])-listOfData[[1]])
  }
  return(list(MSE,fc_values,varPredictors))
}


speedlm.fit<-function (y, X, intercept = FALSE, offset = NULL, row.chunk = NULL, 
                       sparselim = 0.9, camp = 0.01, eigendec = TRUE, tol.solve = .Machine$double.eps, 
                       sparse = NULL, tol.values = 1e-07, tol.vectors = 1e-07, method = c("eigen", 
                                                                                          "Cholesky", "qr"), ...) 
{
  method <- match.arg(method)
  nvar <- ncol(X)
  nobs <- nrow(X)
  if (is.null(offset)) 
    offset <- rep(0, nobs)
  colnam <- colnames(X)
  if (is.null(sparse)) 
    sparse <- is.sparse(X = X, sparselim, camp)
  if (sparse) 
    X <- as(X, "dgCMatrix")
  A <- if (sparse | is.null(row.chunk)) 
    crossprod(X)
  else cp(X,  row.chunk)
  y <- y - offset
  Xy <- if (sparse) 
    crossprod(X, y)
  else t(crossprod(y, X))
  X1X <- colSums(X)
  names(X1X) <- colnam
  yy <- crossprod(y)
  method <- match.arg(method)
  if (method == "eigen") {
    ris <- if (eigendec) 
      control(A,  tol.values, tol.vectors,  method)
    else list(XTX = A, rank = nvar, pivot = 1:nvar)
    ris$XTX <- if (sparse) 
      as(ris$XTX, "dgCMatrix")
    else as(ris$XTX, "matrix")
    ok <- ris$pivot[1:ris$rank]
    if (dim(ris$XTX)[1]==0){print("heck")
      print(ris$XTX)
      print(Xy[ok])}
    coef <- as(solve(ris$XTX, Xy[ok],tol =  tol.solve), "numeric")
    coefficients <- rep(NA, nvar)
    coefficients[ok] <- coef
    RSS <- yy - 2 * crossprod(coef, Xy[ok]) + t(coef) %*% 
      ris$XTX %*% coef
  }
  else if (method == "Cholesky") {
    ris <- if (eigendec) 
      control(A,  tol.values, tol.vectors,  method)
    else list(XTX = A, rank = nvar, pivot = 1:nvar)
    ris$XTX <- if (sparse) 
      as(ris$XTX, "dgCMatrix")
    else as(ris$XTX, "matrix")
    ok <- ris$pivot[1:ris$rank]
    coef <- as(solve(ris$XTX, Xy[ok]), "numeric")
    coefficients <- rep(NA, nvar)
    coefficients[ok] <- coef
    RSS <- yy - 2 * crossprod(coef, Xy[ok]) + t(coef) %*% 
      ris$XTX %*% coef
  }
  else if (method == "qr") {
    if (class(A) == "dsCMatrix") {
      A <- as(A, "matrix")
      Xy <- as(Xy, "matrix")
    }
    C_Cdqrls <- getNativeSymbolInfo("Cdqrls", PACKAGE = getLoadedDLLs()$stats)
    ris <- c(list(XTX = A), .Call(C_Cdqrls, A, Xy, tol.values, 
                                  FALSE))
    coefficients <- ris$coefficients
    coef <- coefficients[ris$pivot[1:ris$rank]]
    ord <- ris$pivot
    RSS <- yy - 2 * crossprod(coefficients, Xy[ris$pivot]) + 
      t(coefficients[ord]) %*% ris$XTX %*% coefficients[ord]
    ok <- ris$pivot[1:ris$rank]
    if (ris$rank < nvar) 
      coefficients[(ris$rank + 1L):nvar] <- NA
    coefficients <- coefficients[ord]
  }
  else stop("speedlm.fit: Unknown method value")
  names(coefficients) <- colnames(X)
  dfr <- nrow(X) - ris$rank
  rval <- list(coefficients = coefficients, coef = coef, df.residual = dfr, 
               XTX = as(ris$XTX, "matrix"), Xy = Xy, nobs = nrow(X), 
               nvar = nvar, ok = ok, A = as(A, "matrix"), RSS = as.numeric(RSS), 
               rank = ris$rank, pivot = ris$pivot, sparse = sparse, 
               yy = yy, X1X = X1X, intercept = intercept, method = method)
  class(rval) <- "speedlm"
  rval
}


FormMSE<- function(models){
  MSE<-models[[1]][[1]]
  colnames(MSE)<-paste("h=",1:maxHorizon)
  rownames(MSE)<-names(models)[1]
  for ( i in 2:length(models)){
    MSE<-rbind(MSE,models[[i]][[1]])
  }
  for(i in 2:length(names(models))){
    rownames(MSE)[i]<-names(models)[i]
  }
  MSE_raw<-MSE[!grepl("stationary",rownames(MSE)),]
  MSE_stationary<-MSE[grepl("stationary",rownames(MSE)),]
  relativeMSEStationary<- t(t(MSE_stationary)/MSE_stationary[1,])
  relativeMSERaw<-t(t(MSE_raw)/MSE_raw[1,])
  output <- list("All"=MSE,"Raw"=MSE_raw, "Stationary"=MSE_stationary,"RelRaw"=relativeMSERaw,"RelStationary"=relativeMSEStationary)
  return(output)
}
  
FindBest <- function(MSEList, ifraw){
  step<-rep(0,3)
  if (ifraw){workMatrix<-MSEList[[4]]}
  else{
    workMatrix<-MSEList[[5]]
  }
  workMatrix[which(workMatrix==0)]<-1000
  step[1]<-which.min(workMatrix[,1])
  step[2]<-which.min(workMatrix[,4])
  step[3]<-which.min(workMatrix[,8])
  out<- c()
  for (i in rownames(workMatrix)[step])
    out<-c(out,which(i==rownames(MSEList[[1]])))
  return(out)}


ApplyDMstatistics<-function(listOfModels,stationary = TRUE){
  if (stationary) {checkString <- "stationary"}
  else {checkString<- "raw"}
  worklist<-listOfModels[grepl(checkString,names(listOfModels))]
  output<-matrix(0,nrow = length(worklist)-1,ncol = ncol(worklist[[1]][[1]]))
  rownames(output)<-names(worklist)[-1]
  colnames(output)<-paste("step",1:8)
  for( i in 1:ncol(output)){
    fc_AR2<-worklist[[grep(checkString, names(worklist))[1]]][[2]][[i]]
    if (stationary) {real<-window(stationaryFullSeries, start = start(fc_AR2),frequency = quartelyFrequency)}
    else {real<-window(fullSeries, start = start(fc_AR2),frequency = quartelyFrequency)}
    e_AR2<-real-fc_AR2
    for(j in 1:nrow(output)){
    if(grepl("lagged CPI",names(worklist)[j+1]) & i>5) {next}

      fc<-worklist[[grep(checkString, names(worklist))[1+j]]][[2]][[i]]
      e<-real-fc
      DMresult<-dm.test(e_AR2,e,h = i,alternative = "greater")
      output[j,i]<-DMresult$p.value
      }
  }
  return(output)
  }
  stars1<-function(matrixRel, matrixDM){
    output<-matrix(0,nrow=dim(matrixDM)[1], ncol = dim(matrixDM)[2])
    for ( i in 1:dim(matrixDM)[1]){
      for( j in 1:dim(matrixDM)[2]){
         if(matrixRel[i+1,j]==0){output[i,j]<-"-"}
        else if (matrixDM[i,j]>0.1){output[i,j]<-paste0(round(matrixRel[i+1,j],3))}
      else if(matrixDM[i,j]>0.5){output[i,j]<-paste0(round(matrixRel[i+1,j],3),"*")}
    else if(matrixDM[i,j]>0.01){output[i,j]<-paste0(round(matrixRel[i+1,j],3),"**")}
          else{output[i,j]<-paste0(round(matrixRel[i+1,j],3),"***") }
      }
    }
    rownames(output)<-rownames(matrixDM)
    rownames(output)<-gsub(pattern = ", raw data",replacement = "",x = rownames(output))
    rownames(output)<-gsub(pattern = ", stationary",replacement = "",x = rownames(output))
    colnames(output)<-colnames(matrixDM)
    return(output)
  }


  
  
  PlottingBest<-function(realSeries,nameOffile,vectorNumBestModels,listOfModels,isTransformed){
    if(isTransformed)
    {yLab="transformed CPI"
      benchmarkNum<-1
      posLegend ="topright"}
    else{ yLab= "CPI"
    benchmarkNum<-2
    posLegend ="bottomright"}
    Width<-9
    Height<-6
    LWD<-rep(2,3)
    LTY<-c(1,5,4)
    real<-zoo(window(realSeries,start= c(1975,1), frequency= quartelyFrequency))
    forecast_1<-zoo(listOfModels[[vectorNumBestModels[1]]][[2]][[1]])
    benchmark<-zoo(listOfModels[[benchmarkNum]][[2]][[1]])
    pdf(paste0(nameOffile,"1.pdf"),height=Height, width=Width)
    plot.zoo(cbind(real, forecast_1,benchmark), 
             plot.type = "single", 
             ylab = yLab,main = rownames(MSE$All)[vectorNumBestModels[1]],xlab = "Year",lty = LTY,lwd = LWD,col = c("red", "blue","black"))
    legend(posLegend, c("Real", "Forecast h=1","AR(2)"), lty = LTY,lwd = LWD, col = c("red", "blue","black"))
    dev.off()
    
    pdf(paste0(nameOffile,"4.pdf"),height=Height, width=Width)
    real<-zoo(window(realSeries,start= c(1976,1), frequency= quartelyFrequency))
    forecast_2<-zoo(listOfModels[[vectorNumBestModels[2]]][[2]][[4]])
    benchmark<-zoo(listOfModels[[benchmarkNum]][[2]][[4]])
    plot.zoo(cbind(real, forecast_2,benchmark), 
             plot.type = "single", 
             ylab = yLab,main = rownames(MSE$All)[vectorNumBestModels[2]],xlab = "Year",lty = LTY,lwd = LWD, col = c("red", "blue","black"))
    legend(posLegend, c("Real", "Forecast h=4","AR(2)"), lty = LTY,lwd = LWD, col = c("red", "blue","black"))
    dev.off()
    
    pdf(paste0(nameOffile,"8.pdf"),height=Height, width=Width)
    real<-zoo(window(realSeries,start= c(1976,4), frequency= quartelyFrequency))
    forecast_3<-zoo(listOfModels[[vectorNumBestModels[3]]][[2]][[8]])
    benchmark<- zoo(listOfModels[[benchmarkNum]][[2]][[8]])
    plot.zoo(cbind(real, forecast_3,benchmark), 
             plot.type = "single", 
             ylab = yLab,main = rownames(MSE$All)[vectorNumBestModels[3]],xlab = "Year",lty = LTY,lwd =LWD,col = c("red", "blue","black"))
    legend(posLegend, c("Real", "Forecast h=8","AR(2)"), lty = LTY,lwd =LWD, col = c("red", "blue","black"))
    dev.off()
  }
  outliersZ <- function(data){
    data<-data[,1]
    if(length(data)<20)zCutOff = 1.036
    else zCutOff=1.282
    #compute standard deviation (sample version n = n [not n-1])
    stdev <- sqrt(sum((data - mean(data, na.rm = T))^2, na.rm = T) / sum(!is.na(data)))
    #compute absolute z values for each value
    absZ <- (data - mean(data, na.rm = T)) / stdev
    #subset data that has absZ greater than the zCutOff and replace them with replace
    #can also replace with other values (such as max/mean of data)
    return((absZ > zCutOff))
  }
  
  cbind.fill <- function(...){
    nm <- list(...) 
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
      rbind(x, matrix(, n-nrow(x), ncol(x))))) 
  }
  
  FindPredictors<- function(vectorIndexes)
  {
    Width<-8
    Height<-6
    setOffreq<-data.frame(listOfModels[[vectorIndexes[1]]][[3]])
    colnames(setOffreq)<-"Frq"
    data_favors<-data.frame(rownames(listOfModels[[vectorIndexes[1]]][[3]])[outliersZ(setOffreq)])
    if(nrow(setOffreq)<20) Info<-", top 15%"
    else Info<-", top 10%"
    rownames(setOffreq)<-gsub(pattern = "`",replacement = "",x =rownames(setOffreq))
    rownames(setOffreq)<-gsub(pattern = "_",replacement = "(",x =rownames(setOffreq))
    rownames(setOffreq)<-paste0( rownames(setOffreq),")")
    q<-ggplot(data = setOffreq,aes(x=rownames(setOffreq),y=Frq,label=rownames(setOffreq)),yla)+geom_point()+geom_text(alpha=0.7,hjust = 0, nudge_x = 0.07,nudge_y = 0.5,aes(size = Frq,colour=outliersZ(setOffreq)))    +labs(x= "All variables", y = "Frequency")+scale_colour_discrete(l = 40) +theme(legend.position = "none",axis.text.x=element_blank(), axis.ticks.x=element_blank())+ggtitle(rownames(MSE$All)[vectorIndexes[1]])
    ggsave(plot= q,paste0(rownames(MSE$All)[vectorIndexes[1]],".pdf"),device = "pdf",width= Width,height= Height)
    colnames(data_favors)[ncol(data_favors)]<-paste(rownames(MSE$All)[vectorIndexes[1]],Info)
    for( i in vectorIndexes[-1]){
      setOffreq<-data.frame(listOfModels[[i]][[3]])
      rownames(setOffreq)<-gsub(pattern = "`",replacement = "",x =rownames(setOffreq))
      rownames(setOffreq)<-gsub(pattern = "_",replacement = "(",x =rownames(setOffreq))
      if(grepl(x=rownames(setOffreq)[1],pattern = "t-"))
      rownames(setOffreq)<-paste0( rownames(setOffreq),")")
      colnames(setOffreq)<-"Frq"
      if(nrow(setOffreq)<20) Info<-", top 15%"
      else Info<-", top 10%"
      data_favors<-cbind.fill(data_favors,rownames(listOfModels[[i]][[3]])[outliersZ(setOffreq)])
      q<-ggplot(data = setOffreq,aes(x=rownames(setOffreq),y=Frq,label=rownames(setOffreq)),yla)+geom_point()+geom_text(alpha=0.7,hjust = 0, nudge_x = 0.07,nudge_y = 0.5,aes(size = Frq,colour=outliersZ(setOffreq)))    +labs(x= "All variables", y = "Frequency")+scale_colour_discrete(l = 40) +theme(legend.position = "none",axis.text.x=element_blank(), axis.ticks.x=element_blank())+ggtitle(rownames(MSE$All)[i])
      ggsave(plot= q,paste0(rownames(MSE$All)[i],".pdf"),device = "pdf",width= Width,height= Height)
      colnames(data_favors)[ncol(data_favors)]<-paste(rownames(MSE$All)[i],Info)
    }
    data_favors<-apply(X = data_favors,MARGIN = 2,FUN = gsub,pattern = "`",replacement = "")
    data_favors<-apply(    X = data_favors,MARGIN = 2,FUN = gsub,pattern="_", replacement = "(")
    data_favors<-cbind(data_favors[,!grepl(pattern = "t-",x = data_favors[1,])],apply(X = data_favors[,grepl(pattern = "t-",x = data_favors[1,])],MARGIN = 2,FUN = paste0,")"))
    return(data_favors)
  }
  