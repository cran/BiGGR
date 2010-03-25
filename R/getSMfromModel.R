#Gets the stochiometric matrix from the model file
getSMfromModel<-function(modelFile,uptake){
    lim<-modelFile
    LP<-Linp(lim)
    #stop(paste("Model file in not correct format: check the LIM package documentation"))
    colnames(lim$A)<-colnames(LP$X) #assign colnames of reaction to the stochiometric matrix
    rnamesSM<-as.character(lim$Components[,1])   #create rownames for the SM except for the last row which is the uptake
    #rnamesSM<-paste("M_",rnamesSM,sep="")
    rnamesSM<-c(rnamesSM,uptake)    
    row.names(lim$A)<-rnamesSM  #assign the constructed row names to the SM    
    sm<-lim$A
    return(sm)
}
