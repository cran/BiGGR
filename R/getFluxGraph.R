######################### Settings for Graph vizualization ####################################
#Adapted from the hypergraph representation of Paul Murrell's efforts for the fluxs

getFluxGraph<-function(gnel,rates,levels,layout){

        names<-names(rates)
        labels<-as.character(rates)
        names(labels)<-names
        
        dz_rates<-discretize(rates,"globalequalwidth", nbins= levels)#discretize the rates based on global equal width
                
        weights<-as.character(dz_rates)
        names(weights)<-colnames(dz_rates)
   
            ###############
            # Create Ragraph object
           
                ragnel <- agopen(gnel, name="whatever",layoutType=layout)
                edges <- edgeNames(gnel)

            #~~~~~~~~~~~~~~Changes end here~~~~~~~~~~~~~~~~
             testbph <- graphBPH(gnel,"^R")
             testrabph <- graphLayout(testbph)
             edgeDataDefaults(testrabph, "color") <- "black"   # default values
             edgeDataDefaults(testrabph, "lwd") <- 1
             nodeDataDefaults(testrabph, "margin") <- 'unit(3, "mm")'
             nodeDataDefaults(testrabph, "shape") <- "box"
            graphDataDefaults(testrabph, "arrowLoc") <- "middle"
            graphData(testrabph, "arrowLoc") <- "end"
            graphData(testrabph, "arrowLoc") <- "none"

            #~~~~~~~~~~~~~~Changes end here~~~~~~~~~~~~~~~~

                for (i in names) {
                    modEdges <- edges[grep(i, edges)]
                    for (e in modEdges) {
                        ends <- strsplit(e, "~")[[1]]
                 edgeData(testrabph, ends[1], ends[2], "lwd") <- weights[i]
                         edgeData(testrabph, ends[1], ends[2], "color") <-
                            if (as.numeric(labels[i]) < 0) "red" else "blue"

                    }
                }
        return(testrabph)
}
