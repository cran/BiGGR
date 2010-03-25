#The following function gets the adjacency matrix from stochiometric matrix in a proper format also taking the negative values into account

getAdjMfromStoM<-function(sm,react_name,minimal){

    sm_react_name<-sm[,react_name]
    ss<-sm_react_name[rowSums(sm_react_name) != 0,] #remove all rows having all "zeros"
    ss_temp<-ss

    index<-which(ss_temp < 0) # get all the rows which have a negative value
    ss_temp[index]<-1   #matrix without negative values dummy incidence matrix

    ind<-which(ss_temp > 0) #get all the rows which have value greater than 1
    ss_temp[ind]<- 1       #set this values to 1

    node_names<-row.names(ss_temp)
    edge_names<-colnames(ss_temp)
    ig<-graph.incidence(ss_temp,directed=TRUE)
    edg_list<-get.edgelist(ig, names=TRUE)
    gnel<-ftM2graphNEL(edg_list,edgemode="directed")
    adjM<-as(gnel,"matrix")

    cst<-colnames(ss)
    rst<-row.names(ss)

    #####give directions according to the stochiometric rules##########
    for(i in 1:length(rst)){
        for(j in 1:length(cst)){
            if(ss[rst[i],cst[j]]>0){
                adjM[rst[i],cst[j]]<-0
                adjM[cst[j],rst[i]]<-1
            }
    
        }

    }
    
    if(minimal==FALSE){
        return(adjM)
    }else{
        vec<-c("^M_h2o","^M_pi","^M_adp","^M_atp","^M_nadh","^M_nadp","^M_co2","^M_o2","^M_nad","^M_h","^M_fad","M_gdp","M_gtp","M_fadh2")
        for (k in 1:length(vec)){
            a<-grep(vec[k],colnames(adjM))
            adjM<-adjM[-a,-a]
        }
        return(adjM)
    }
    
    
}
