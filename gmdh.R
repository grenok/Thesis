#source("ForecastSupportFunctions.R")
library(corpcor)
#pseudoinverse is a function from the package corpcor
pseudoinverse<- function (m, tol) 
{
  msvd = fast.svd(m, tol)
  if (length(msvd$d) == 0) {
    return(array(0, dim(m)[2:1]))
  }
  else {
    return(msvd$v %*% (1/msvd$d * t(msvd$u)))
  }
}
OLSS<-function(X_var,Y_var){
  g<-t(X_var[,1])%*%Y_var
  teta<-(1/t(X_var[,1])%*%X_var[,1])%*%g
  X<-X_var[,1]
  for(i in 2:(ncol(X_var))){
    X<-cbind(X,X_var[,i])
    g<-cbind(g,t(X_var[,i])%*%Y_var)
    teta<-pseudoinverse(t(X)%*%X, tol= 1e-40)%*%t(g)
  }
  return(teta)
  
}
MatrixFromLayer <- function(Layer){
  output<-t(Layer[,-c(1:3)])
}

ResultOfLayer<- function(listOfLayers, Data, i){
  if(i>1){Data <-ResultOfLayer(listOfLayers,Data,(i-1))}
  if(nrow(listOfLayers[[i]])>1){coefs<-t(as.matrix(listOfLayers[[i]][,-(1:3)]))}
  else{coefs<-as.matrix(listOfLayers[[i]][,-(1:3)])}
    #dimentions of output Data matrix
    n<-nrow(Data)
    m<-nrow(listOfLayers[[i]])
    NewData<-matrix(nrow = n, ncol = m)
    for(l in seq(m)){
      k<-listOfLayers[[i]][l,2]
      j<-listOfLayers[[i]][l,3]
      NewData[,l]<-matrix(c(rep(1,nrow(Data)),Data[,k],Data[,j],Data[,k]*Data[,j]),ncol = numberOftermsInFunction)%*%coefs[,l]
 

    }
    return(NewData)
}
GMDH<- function(IndependentData, DependentVector, Split ){
  net<-list(as.matrix(1:3))
  iter<-1
  
  LastLayerOutput<-IndependentData
  error<-1e+10
  while(iter==1 || ( iter<3 && error>0.001)){
    if(iter>1){if(nrow(net[[iter-1]])==1){break}}
 #while(!(iter!=1 && error<net[[iter-1]][which.min(net[[iter-1]][,1])] || iter<9 || error>0.000001) && nrow(net[[iter]])!=1) {
    net[[iter]]<-FindLayer(LastLayerOutput,DependentVector, Split)
    oldError<-error
    error<-net[[iter]][which.min(net[[iter]][,1])]
    if(iter>1){if(oldError< error){
     # iter<- iter-1
      break}}
    if(nrow(net[[iter]])>1){coefs<-t(as.matrix(net[[iter]][,-(1:3)]))}
    else{coefs<-as.matrix(net[[iter]][,-(1:3)])}
    #dimentions of output Data matrix
    n<-nrow(IndependentData)
    m<-nrow(net[[iter]])
    LastLayer<-LastLayerOutput
    LastLayerOutput<-matrix(nrow = n, ncol = m)
    for(l in seq(m)){
      k<-net[[iter]][l,2]
      j<-net[[iter]][l,3]
      LastLayerOutput[,l]<-matrix(c(rep(1,n),LastLayer[,k],LastLayer[,j],LastLayer[,k]*LastLayer[,j]),ncol = numberOftermsInFunction)%*%coefs[,l]
    }
    iter<-iter+1
  }
  return(net)
}

FindLayer<- function(MatrixOfData,  DependentVector,vectorForLearning){
  #vectorForLearning is a bool vector indicating as TRUE those vintages which take part in learning
  NumberOfsurvivors = 4 #Some number which we like - number of outputs which survaive on each layer
  numberOfNodes <-max((ncol(MatrixOfData))*(ncol(MatrixOfData)-1)/2,1)
  numberOftermsInFunction<<- 4# f(x,y)= a0+a1*x+a2*y+a3*x*y
  NumberOfAdditionalCells <- 3
  Layer <- matrix(rep(0, (numberOftermsInFunction+NumberOfAdditionalCells)*numberOfNodes), nrow = numberOfNodes) 
  #here 3 is allocated for MSE of the node, and numbers of variables which are inputs for the node
  nodeNumber <- 1
  for( i in seq(ncol(MatrixOfData)-1)){
    for( j in (i+1): ncol(MatrixOfData)){
      #Creating a matrix A from a matrix representation Ax=y, where y is a output of a certain node, 
      #A is created out of two vectors which are inputs for a node
      MatrixOfTerms <- matrix(c(rep(1,nrow(MatrixOfData)),MatrixOfData[,i],MatrixOfData[,j],MatrixOfData[,i]*MatrixOfData[,j]),ncol = numberOftermsInFunction)
      beta <- pseudoinverse(t(MatrixOfTerms[vectorForLearning,])%*%MatrixOfTerms[vectorForLearning,], tol=1e-40)%*%t(MatrixOfTerms[vectorForLearning,])%*%DependentVector[vectorForLearning] 
      #beta <- OLSS(MatrixOfTerms[vectorForLearning,],DependentVector[vectorForLearning])
      Layer[nodeNumber, ] <-c(mean((MatrixOfTerms[ !vectorForLearning, ]%*%beta - DependentVector[!vectorForLearning])^2),i,j,beta)
      nodeNumber <- nodeNumber+1
    }
  }
  if(nrow(Layer)==1){return(Layer)}else{
 return(Layer[sort(Layer[,1])[min(NumberOfsurvivors,numberOfNodes)]>=Layer[,1],])}
}

RetrievePredictiveVariables<- function(net, InpependentData){
 indexes<-c()
  for (i in 1:nrow(net[[1]])){
    for(j in 2:3){
    #indexes<-c(indexes,net[[1]][i,j])
      if(!(net[[1]][i,j] %in% indexes)){indexes<-c(indexes,net[[1]][i,j])}
    }
  }
return(colnames(InpependentData)[indexes])
}

CountingPredictiveVariables<-function(net, data, listOfNames){
  names<-RetrievePredictiveVariables(net = net,InpependentData = data)
  for (item in names){
    if(length(listOfNames)>0){
      if(!(item %in% rownames(listOfNames))){
        listOfNames<-rbind(listOfNames,1)
        rownames(listOfNames)[nrow(listOfNames)]<-item
      }else{listOfNames[item,]<-listOfNames[item,]+1}
    }else{listOfNames<-data.frame(1)
    rownames(listOfNames)<-item
    }
  }
  return(listOfNames)
}

#function form the package caTools:
sample.split<-function (Y, SplitRatio = 2/3, group = NULL) 
{
  nSamp = length(Y)
  nGroup = length(group)
  if (nGroup > 0 && nGroup != nSamp) 
    stop("Error in sample.split: Vectors 'Y' and 'group' have to have the same length")
  BinOne = logical(nSamp)
  SplitRatio = abs(SplitRatio)
  if (SplitRatio >= nSamp) 
    stop("Error in sample.split: 'SplitRatio' parameter has to be i [0, 1] range or [1, length(Y)] range")
  U = unique(Y)
  nU = length(U)
  if (2 * nU > nSamp | nU == 1) {
    n = if (SplitRatio >= 1) 
      SplitRatio
    else SplitRatio * nSamp
    rnd = runif(nSamp)
    if (nGroup) 
      split(rnd, group) <- lapply(split(rnd, group), mean)
    ord = order(rnd)
    BinOne[ord[1:n]] = TRUE
  }
  else {
    rat = if (SplitRatio >= 1) 
      SplitRatio/nSamp
    else SplitRatio
    for (iU in 1:nU) {
      idx = which(Y == U[iU])
      n = round(length(idx) * rat)
      rnd = runif(length(idx))
      if (nGroup) {
        grp = group[idx]
        split(rnd, grp) <- lapply(split(rnd, grp), mean)
      }
      ord = order(rnd)
      BinOne[idx[ord[1:n]]] = TRUE
    }
  }
  if (SplitRatio >= 1) {
    n = sum(BinOne) - SplitRatio
    if (n > 0) 
      BinOne[sample(which(BinOne), n)] = FALSE
    else if (n < 0) 
      BinOne[sample(which(!BinOne), -n)] = TRUE
  }
  return(BinOne)
}

CountTotalParticipation<- function(DataWithFrequencies,MainDataSet){
  output<-data.frame(row.names=colnames(MainDataSet))
  output[,1]<-0
  for(i in 1:nrow(output)){
    for ( j in 1:nrow(DataWithFrequencies)){
    if(grepl(rownames(output)[i], rownames(DataWithFrequencies)[j])){
      output[i,1]<-output[i,1]+DataWithFrequencies[[1]][j]
    }
      
    }
  }
  return(output)
    
}

ResultOfnet<-function(net, data){
  return(ResultOfLayer(net,data,length(net)-1)[which.min(net[[length(net)-1]][,1])])
}

# 
# t_data<-list()
# t_data[[1]]<-laggedDataRich[[1]]
# for(i in 2:length(laggedDataRich)){
#   t_data[[i]]<-t(t(laggedDataRich[[i]])/apply(laggedDataRich[[i]],2,max))
# }
# 

CountingPredictivPairs<-function(net, data, listOfNames){
  pairNames<-list()
  for (i in 1:nrow(net[[1]])){
       pairNames[[i]]<-paste(colnames(data)[c(net[[1]][i,2],net[[1]][i,3])],collapse = " ")
  }
  
  for (item in pairNames){
      if(length(listOfNames)>0){
        if(!(item %in% rownames(listOfNames))){
          listOfNames<-rbind(listOfNames,1)
          rownames(listOfNames)[nrow(listOfNames)]<-item
        }else{listOfNames[item,]<-listOfNames[item,]+1}
      }else{listOfNames<-data.frame(1)
      rownames(listOfNames)<-item
      }
  }
  return(listOfNames)
}


