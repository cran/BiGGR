#Generate a model file for FBA(flux balance analysis) where file1 is the metabolite file as exported from the Bigg Database and file2 is the reaction file
#maximize is a character string giving name of reaction tag to be maximized, 
#equation_var is a character string indicating the start of the reaction
# equation_value is a numeric value the initial value of the equation_var
# constraints is a character string in the format for example "[0,1000]" where 0 is the minimal and 1000 is maximum
# externals is a character vector provided by user

createBiggModel<-function(metabolites_file,reactions_file,maximize,equation_var,equation_value,constraint,externals){

    #Common externals
    default_externals<-c("h","nad","nadh","pi","fad","fadh2","h2o","adp","atp","nadp","co2","o2","gdp","gtp")
    externals<-c(externals,default_externals)
    
    file1<-read.delim(metabolites_file,sep="\t",header=TRUE)
    file2<-read.delim(reactions_file,sep="\t",header=TRUE)
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
    if(isTRUE(grep("[a-z]-",reactions))==TRUE){
        rr<-grep("[a-z]-",reactions)
        react<-reactions[-rr]
        react1<-reactions[rr]
        rr1<-sub("-","",react1)
        reactions<-c(rr1,react)
    }
   
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
    components<-setdiff(components,externals)
    write(components,file="Model.lim",append=TRUE)
    write("###   END COMPONENTS",file="Model.lim",append=TRUE)


    write("\n\n",file="Model.lim",append=TRUE)
    
    
    #EXTERNALS
    write("###   EXTERNALS",file="Model.lim",append=TRUE)
    write(externals,file="Model.lim",append=TRUE)   #User specified externals for a system
    write("###   END EXTERNALS",file="Model.lim",append=TRUE)
}
