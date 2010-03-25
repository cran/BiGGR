#The following function is adapted from the Setup.liminput from LIM package to get a character vector of possible externals in a set of reactions.
#the input to this function is a list returned from the function "Read" from LIM package

possibleExternals <- function (liminput)  {
  calcvalues <- function(pars, npar) {
    if (npar == 0)
      return(NULL)
    parval <- rep(0, length = npar)
    for (i in 1:npar) {
      term <- pars[pars$nr == i, ]
      for (ii in 1:nrow(term)) {
        parval[i] <- parval[i] + calcterm(term[ii, ])
      }
    }
    return(parval)
  }

  calcterm <- function(term) {
    calc <- term$val
    if (!is.na(term$par1))
      calc <- calc * parval[term$par1]
    if (!is.na(term$par2))
      calc <- calc * parval[term$par2]
    if (!is.na(term$par3))
      calc <- calc * parval[term$par3]
    if (!is.na(term$par4))
      calc <- calc * parval[term$par4]
    if (!is.na(term$comp))
      calc <- calc * compval[term$comp]
    if (!is.na(term$external))
      calc <- calc * externval[term$extern]
    return(calc)
  }


## begin main body...
  ncomp <- length(liminput$compnames)
  nextern <- length(liminput$externnames)
  ifelse (is.null(liminput$rate), nrate <- 0, nrate <- ncomp)
  ifelse (is.null(liminput$marker), nmarker <- 0, nmarker <- nrow(liminput$marker))
  npar <- length(liminput$parnames)
  nvars <- length(liminput$varnames)
  flows <- liminput$flows
  reactions <- liminput$reactions
  ispos <- TRUE
  ifelse (is.null(reactions),nreac <-0, nreac <- max(reactions$nr))
  nflow <- nrow(flows)
   neqs <- nineqs <- 0
  if (is.null(nflow))
    nflow <- 0
  if(liminput$Type == "flow")     Unknown <- "F" else
  if(liminput$Type == "reaction") Unknown <- "R" else Unknown <- "S"
  if (Unknown == "F") {
    nunkn <- nflow
    if (nunkn == 0)
      stop("cannot create inverse matrices: number of flows=0")
    unkncol <- 9
    Unknownnames <- liminput$flows$name
    if (any(!liminput$posreac))
      ispos<-FALSE
  } else if (Unknown == "R") {
    nunkn <- max(reactions$nr)
    if (nunkn == 0)
      stop("cannot create inverse matrices: number of unknowns =0")
    unkncol <- 12
    Unknownnames <- unique(liminput$reactions$name)
    if (any(!liminput$posreac))
      ispos<-FALSE
    } else {
        nunkn <- ncomp
        unkncol <- 10
        if (nunkn == 0)
            stop("cannot create inverse matrices: number of components=0")
        Unknownnames <- liminput$compnames
    }
    parval <- rep(NA, npar)
    compval <- rep(NA, ncomp)

    externval <- calcvalues(liminput$extern, nextern)
    rateval   <- calcvalues(liminput$rate, nrate)
    markerval <- calcvalues(liminput$marker, nmarker)
    if (length(rateval) == 0)
        rateval <- rep(0, ncomp)
    rateval[is.na(rateval)] <- 0
    VarA <- VarB <- NULL

    eqmat <- NULL
    
    ineqmat <- NULL

    cost <- NULL

    profit <- NULL

    A <- B <- G <- H <- NULL

    if (nreac>0)
    {
        A <- matrix(data = 0, nrow = ncomp, ncol = nunkn)
# KARLINE: HERE IS THE NEW SECTION
        for (i in 1:nrow(reactions)) {
            st <- reactions[i,"comp"]
            rr <- reactions[i,"nr"]
            A[st,rr] <- A[st,rr] + reactions[i,"val"]
        }
        B <- rateval
    }

    if (nflow > 0 || nreac>0 ) {
        # CHECK A-matrix for possible EXTERNALS
        Aext <- cbind(A[1:ncomp,],rateval)
        Aext[Aext!=0] <- 1
        RS <- rowSums(Aext)
        if (any (RS<=1))
#         {warning("following component(s) is possibly an external:")
#          warning(paste(" ",liminput$compnames[which(RS<=1)]))}
          
         ###modified here by Anand Gavai 
         #externals_anand<- liminput$compnames[which(RS<=1)]
         ####
          
        # not all reaction rates are positive (i.e. some can go both ways <->)
        G <- diag(nunkn)
        G<- G[liminput$posreac,]
        H <- rep(0,sum(liminput$posreac))
    }

    return(liminput$compnames[which(RS<=1)])
}
