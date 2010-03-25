###########This a function to get rates##############

getRates<-function(modelFile){
    LP<-Linp(modelFile)
    rates<-as.data.frame(LP$X)   #optimized rates from the model file
    return(rates)
}
