
model2hyperdraw<-function(modelFile,uptake,minimal,levels,layout){ #uptake is a vector of external inputs provided to the system
#warning("The uptake vector must consist of all compounds as specified in the EQUATION section of the Model file")
    lim <-Setup(modelFile)
    #lim<-modelFile
    LP<-Linp(lim)
    rates<-getRates(modelFile)
   
    colnames(lim$A)<-colnames(LP$X) #assign colnames of reaction to the stochiometric matrix
    rnamesSM<-as.character(lim$Components[,1])   #create rownames for the SM except for the last row which is the uptake
    #rnamesSM<-paste("M_",rnamesSM,sep="")
    rnamesSM<-c(rnamesSM,uptake)    
    row.names(lim$A)<-rnamesSM  #assign the constructed row names to the SM    
    sm<-lim$A
    
    ss_temp<-sm

    index<-which(ss_temp < 0) # get all the rows which have a negative value
    ss_temp[index]<-1   #matrix without negative values dummy incidence matrix

    ind<-which(ss_temp > 0) #get all the rows which have value greater than 1
    ss_temp[ind]<- 1       #set this values to 1

    node_names<-row.names(ss_temp)
    node_names<-paste("M_",node_names,sep="")
    row.names(sm)<-node_names             #attach abbreviation to the metabolites
    edge_names<-colnames(ss_temp)
    row.names(ss_temp)<-node_names             #attach abbreviation to the metabolites
    ig<-graph.incidence(ss_temp,directed=TRUE)
    edg_list<-get.edgelist(ig, names=TRUE)
    edg_list<-unique(edg_list)
    gnel<-ftM2graphNEL(edg_list,edgemode="directed")
    adjM<-as(gnel,"matrix")

    cst<-colnames(sm)
    rst<-row.names(sm)

    #####give directions according to the stochiometric rules##########
        for(i in 1:length(rst)){
            for(j in 1:length(cst)){
                if(sm[rst[i],cst[j]]>0){
                    adjM[rst[i],cst[j]]<-0
                    adjM[cst[j],rst[i]]<-1
                }
        
            }
    
        }
        
        if(minimal==TRUE){
            vec<-as.matrix(lim$Externals[1])
            vec<-paste("M_",vec,sep="")
            adjM<-data.frame(adjM)       
            index<-which(colnames(adjM) %in% vec == TRUE)
            adjM<-adjM[-index,-index]
        }else{
            adjM = adjM
        }
        adjM<-as.matrix(adjM)
        gnel<-as(adjM,"graphNEL")
        ragnel<-getFluxGraph(gnel,rates,levels,layout)  #Returns an object of type  "RagraphBPH" from package hyperdraw        
}
