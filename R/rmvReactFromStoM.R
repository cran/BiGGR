#The following function removes a reaction from the stochiometric matrix of metabolic reconstructions
#This function should be used only when reactions need to be removed from the stoichiometric matrix



#reactID<-"R_PYK" 

rmvReactFromStoM<-function(sm,reactID){
    #stopifnot(grep(reactID,colnames(sm))==0)
    warning("Make sure reaction Id exist in the Stochiometric Matrix")
    idx<-grep(reactID,colnames(sm))
    csm<-sm[,-idx]

   for(i in 1:dim(csm)[1]){
        length<-length(which(csm[i,]==0)) ## get the length of the "0"s for the rows of the matrix
        if(length==dim(csm)[2]){
            csm<-csm[-i,]
        }  
    }


    return(csm)
}
