\encoding{latin1}
\name{addReact2StoM}
\Rdversion{1.1}
\alias{addReact2StoM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{adds a user defined reaction to stoichiometric matrix}
\description{Adds a user defined reaction to stoichiometric matrix which is not found in the BiGG Database}

\usage{addReact2StoM(sm,reactID,react)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sm}{ is of type incidence matrix representing reconstructions in the database. }
  \item{reactID}{ is of type character string representing the enzyme which catalyzes the reaction. }
  \item{react}{ is of type character string representing the actual reaction with co-efficients for e.g. \code{glucose + ATP -> glucose 6-phosphate + ADP. }
   }
}

\details{
	This function should be used only when a reaction is not found in the reconstructions from BiGG
}
\value{
 \item{sm}{ returns a matrix of the entire reconstruction for an organizm(s) available from the BiGG Database with user defined reaction(s) }
}

\author{ Anand K. Gavai <anand.gavai@bioinformatics.nl> }


\section{Warning }{to be used only when a reaction is not found in the reconstructions}

\seealso{
\code{\link{getAdjMfromStoM}},
}
\examples{
	data("Glycolysis")
	sm<-getSMfromModel(Glycolysis,"R_HEX1")
	sm
	reactID<-"R_ABC"
	react<-"A+B->C+D"
	sm<-addReact2StoM(sm,reactID,react)
	sm	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stoichiometric matrix,add reactions}
