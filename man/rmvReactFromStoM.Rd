\name{rmvReactFromStoM}
\encoding{latin1}
\Rdversion{1.1}
\alias{rmvReactFromStoM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{removes a reaction from the stoichiometric matrix}
\description{Removes an existing reaction from the stoichiometric matrix}

\usage{rmvReactFromStoM(sm,reactID)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sm}{is of type incidence matrix representing reactions. }
  \item{reactID}{is of type character string representing the enzyme which catalyzes the reaction found in the stochiometric matrix. }
}
\details{
	This function should be used only when a reaction needs to be removed from the stochiometric matrix
}
\value{
	 \item{sm}{returns a stoichiometric matrix with selected reaction removed.}
}


\author{Anand K. Gavai <anand.gavai@bioinformatics.nl>}


\section{Warning }{ to be used only when a reaction is not found in the reconstructions }

\seealso{
\code{\link{addReact2StoM}}
}
\examples{
	data("Glycolysis")
	sm<-getSMfromModel(Glycolysis,"R_HEX1")
	sm
	reactID <- "R_PYK"
	csm<-sm
	csm[10,1]<-0
	csm
	sm<-rmvReactFromStoM(sm,reactID)	
	sm
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stoichiometric matrix,remove reaction}
