#Function which adds a reaction to the stochiometric matrix of metabolic reconstructions
#This function should be used only when reactions are not found in the stoichiometric matrix


# For Example:
#    reactID<-"R_ABC" 
#    react<-"A+B->C+D"

addReact2StoM<-function(sm,reactID,react){

    warning("Reaction should be in the format A+B -> C+D")
    react<- gsub("[1-9]", "", react) #strip the stoichiometric coefficients from the reaction
    split<-unlist(strsplit(react,"->"))
    reactants<-split[1]
    substrates<-split[2]
    
    reactID<-paste("R_",reactID,sep="") #attach abbreviations
    met_react<-unlist(strsplit(reactants,"[+]")) #split the string to check to for 
    met_react<-paste("M_",met_react,sep="") #attach abbreviations

    met_sub<-unlist(strsplit(substrates,"[+]"))
    met_sub<-paste("M_",met_sub,sep="")    #attach abbreviations
    
    compounds<-c(met_react,met_sub)
    
    #check if the compounds exist in the stoichiometric matrix
    if((compounds %in% row.names(sm))==FALSE){
        for(i in 1:length(compounds)){
            nv<-as.data.frame(rep(0,dim(sm)[2]))
            nv<-t(nv)
            row.names(nv)<-compounds[i]
            colnames(nv)<-colnames(sm)
            sm<-rbind(sm,nv)
        }
        recv<-as.data.frame(rep(0,dim(sm)[1]))  #reaction empty vector
        colnames(recv)<-reactID 
        row.names(recv)<-row.names(sm)
        sm<-cbind(sm,recv) #bind reaction vector
        sm[met_react,reactID]<- -1      
        sm[met_sub,reactID]<- 1
    }else{        ###  assign the relative positions of reactantants and substrates to the stoichiometric matrix
        sm[met_react,reactID]<- -1      
        sm[met_sub,reactID]<- 1
        }
        return(sm)
}
