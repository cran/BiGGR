# Attach abbreviations to the Reactions with a prefix "R_"

attachReactAbbre<-function(Reactions){
    #met<-metabolites
    #metabolites <- metabolites$abbreviation
    #compartment <- met$compartment
    react<-Reactions$abbreviation #fetch the abbreviations from the file reaction

    react<-as.character(react)
    react<-strsplit(react,",")

    dum<-NULL
    for(i in 1:length(react)){
    	dum<-cbind(dum,react[[i]][1])
    }
 
    dum<-as.character(dum)

    react_name<-paste("R_",dum,sep="") #concatenate the r_ to the reaction abbreviations
    return(react_name)
}
