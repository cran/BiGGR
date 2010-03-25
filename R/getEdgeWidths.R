#Get edge widths for the graph rates is a numeric vector and levels is a numeric integer to divide it into different levels
getEdgeWidths<-function(rates,levels){
    dz_rates<-discretize(rates,"globalequalwidth", nbins= levels) #discretize the rates based on global equal width
    return(dz_rates)
}
