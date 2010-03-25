\name{getFluxGraph}
\encoding{latin1}
\Rdversion{1.1}
\alias{getFluxGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Gets graphical representation of a flux distribution}

\description{Displays graphical representation of flux distribution using the Hypergraph package}
\usage{getFluxGraph(gnel, rates,levels, layout)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gnel}{A \code{graphNEL} object}
  \item{rates}{a numeric vector representing descritized rates of fluxes as estimated from the \code{LIM} package}
  \item{levels}{A numeric value giving the levels of descritization to be performed}
  \item{layout}{A character striing representing layout from standard \code{Graphviz} library for e.g "dot","neato","circo","fdp","sfdp","twopi"}
}

\value{
 \item{gnel}{returns an object of class \code{RagraphBPH} with values 
	     of fluxes attached to edges, to be used to be displayed by \code{hypergraph}}
}

\author{Anand K. Gavai <anand.gavai@bioinformatics.nl> }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link[BiGGR:getRates]{getRates},\link[BiGGR:getEdgeWidths]{getEdgeWidths},\link[BiGGR:getAdjMfromStoM]{getAdjMfromStoM},\link[BiGGR:getStoichiometricMatrix]{getStoichiometricMatrix}
}

\examples{
	\dontrun{
	data("gnel")
	data("Glycolysis")
	rates<-getRates(gnel)
	levels<-7
	layout<-"dot"
	ragnel<-getFluxGraph(gnel,rates,levels,layout)
	ragnel
   }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Hypergraph}
\keyword{Hyperdraw}
\keyword{Bipartite Graph}
