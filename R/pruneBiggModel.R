#This function takes the modelFile and reshaphe it taking into account the non connection species of the reactions

pruneBiggModel<-function(modelFile,metabolites,reactions,maximize,equation_var,equation_value,constraint,externals){
    warning("Make sure the model file is first created using the createModel function")
    liminput<-Read(modelFile)
    liminput$compnames<-liminput$compnames
    prune_ext<-tolower(possibleExternals(liminput))
    
    
        #Common externals
    default_externals<-c("h","nad","nadh","pi","fad","fadh2","h2o","adp","atp","nadp","co2","o2","gdp","gtp")
    externals<-unique(tolower(c(externals,default_externals,prune_ext)))
    
    file1<-read.csv(metabolites)
    file2<-read.csv(reactions)
    react_tag<-attachReactAbbre(file2) #construct a reaction tag
    equations<-file2$equation           #fetch the reactions
    equations<-sub("-->","->",equations)
    equations<-sub("<==>","<->",equations)
    equations<-strsplit(equations, ":", fixed = TRUE)
    equations<-t(as.data.frame(equations))
    row.names(equations)<-NULL

    #REACTIONS
    write("###   REACTION",file="Model.lim")
    reactions<-paste(react_tag,":",equations[,2])
    reactions<-gsub("[(]","",reactions) #remove the opening brace
    reactions<- gsub("[)]","*",reactions) #remove the closing brace and replace it with * which is the stochiometric coefficient
    reactions<-gsub("[[a-z]]","",reactions) #remove the square brackets giving the compartment information
    reactions<-gsub("[[]","",reactions) # remove the remaining bracket from the above
    
    #check reactions consisting of hyphens
    if(as.numeric(grep("[a-z]-",reactions))>0){
        rr<-grep("[a-z]-",reactions)
        react<-reactions[-rr]
        react1<-reactions[rr]
        rr1<-sub("-","",react1)
        reactions<-c(rr1,react)
    }
   
    reactions<-reactions
    write(reactions,file="Model.lim",append=TRUE)
    write("### END REACTION",file="Model.lim",append=TRUE)
    
    write("\n\n",file="Model.lim",append=TRUE)
    
    #OPTIMIZE MINIMUM OR MAXIMUM REACTION
    write("### MAXIMISE",file="Model.lim",append=TRUE)
    var_name<-paste("max_",maximize,sep="")
    write(paste(var_name,":",maximize),file="Model.lim",append=TRUE) 
    write("### END MAXIMISE",file="Model.lim",append=TRUE)
    
    write("\n\n",file="Model.lim",append=TRUE)
    
    #CONSTRAINTS
    write("###   CONSTRAINTS",file="Model.lim",append=TRUE)
    write(paste(maximize,"=",constraint),file="Model.lim",append=TRUE)
    write("###   END CONSTRAINTS",file="Model.lim",append=TRUE) 


    write("\n\n",file="Model.lim",append=TRUE)
    
    
    #EQUATIONS
    write("###   EQUATIONS",file="Model.lim",append=TRUE)
    write(paste(equation_var,"=",equation_value),file="Model.lim",append=TRUE) 
    write("###   END EQUATIONS",file="Model.lim",append=TRUE) 
    
    write("\n\n",file="Model.lim",append=TRUE)
    
    
    #COMPONENTS
    write("###   COMPONENTS",file="Model.lim",append=TRUE)
    components<-as.character(file1$abbreviation)  #derived from the metabolite list for a specific pathway from Bigg Database
    components<- gsub("-","",components)
    components<-setdiff(tolower(components),tolower(externals))
    components<-setdiff(tolower(components),tolower(prune_ext))
    write(components,file="Model.lim",append=TRUE)
    write("###   END COMPONENTS",file="Model.lim",append=TRUE)


    write("\n\n",file="Model.lim",append=TRUE)
    
    
    #EXTERNALS
    write("###   EXTERNALS",file="Model.lim",append=TRUE)
    write(externals,file="Model.lim",append=TRUE)   #User specified externals for a system
    write("###   END EXTERNALS",file="Model.lim",append=TRUE)

    
}
