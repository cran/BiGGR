\name{getAdjMfromStoM}
\encoding{latin1}

\Rdversion{1.1}

\alias{getAdjMfromStoM}

\title{getAdjMfromStoM}
\description{constructs an adjacency matrix from a stoichiometric matrix}
\usage{getAdjMfromStoM(sm, react_name, minimal)}

\arguments{

  \item{sm}{is a stoichiometric matrix (or incidence matrix) for the entire metabolic reconstruction for a given organism, as provided in the datasets of this package see \code{getStoichiometricMatrix}}

  \item{react_name}{is a character vector provided by the \code{reactions} file as exported from (BiGG) at \url{http://bigg.ucsd.edu/bigg/main.pl} for a specific pathways of interest.}

  \item{minimal}{logical if set to TRUE would leave out common biproducts of reactions for example: atp,adp,nadh,nadh2 etc. from the visualization}
}

\value{
 \item{adjM}{returns an object of class \code{matrix} for the selected reactions}
}
\author{Anand K. Gavai <anand.gavai@bioinformatics.nl>}


\seealso{
	\link[BiGGR:getStoichiometricMatrix]{getStoichiometricMatrix}
}
\examples{
\dontrun{
	data("Glycolysis")
	uptake<-"R_HEX1"
	sm<-getSMfromModel(Glycolysis,uptake)
	sm
	react_name<- attachReactAbbre("/examples/Reactions.txt")	#attach abbreviations
	adjM<- getAdjMfromStoM(sm,react_name,"TRUE")
	adjM #adjacency matrix or incidence matrix for the selected pathway
	}
}

\keyword{Stoichiometric matrix}
\keyword{Reaction matrix}
