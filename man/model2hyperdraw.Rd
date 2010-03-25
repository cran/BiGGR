\name{model2hyperdraw}
\encoding{latin1}
\Rdversion{1.1}
\alias{model2hyperdraw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draws a hypergraph representation from a model file}
\description{Convert a model file to a \code{hypergraph} representation}
\usage{model2hyperdraw(modelFile,uptake,minimal,levels,layout)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelFile}{is a file created from \code{createModel} or \code{pruneModel}}
  \item{uptake}{is a character vector representing the substrate uptake of in a metabolic process}
  \item{minimal}{is a logical value TRUE or FALSE to visualize externals on a graph}
  \item{levels}{is a numeric value to determine the levels of thickness of edges}
  \item{layout}{is a character string representing the layout engine to be used for visualization for example "dot", "twopi","neato","fdp","sfdp" and "circo"}
}

\value{
 \item{graphNEL object}{returns an \code{graphNEL} object representation. }
}

\author{Anand K. Gavai <anand.gavai@bioinformatics.nl>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\link[hyperdraw:graphBPH]{graphBPH}
}
\examples{
    data("Glycolysis")
    uptake<-"glcD"
    minimal<-"TRUE"
    levels<-7
    layout<-"neato"
    gnel<-model2hyperdraw(Glycolysis,"glcD",TRUE,levels,layout)
    gnel
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hypergraph}
\keyword{hyperdraw}
