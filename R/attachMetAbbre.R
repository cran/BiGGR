# Attach abbreviations to the Metabolites with a prefix "M_"

attachMetAbbre<-function(Metabolites){
    met<-Metabolites
    met<-as.character(met[,1])

    met<-strsplit(met,",")

    dum<-NULL
    for(i in 1:length(met)){
    	dum<-cbind(dum,met[[i]][1])
    }
    dum<-as.character(dum)

    met_name<-paste("M_",dum,sep="") #concatenate the r_ to the reaction abbreviations
    return(met_name)
}
