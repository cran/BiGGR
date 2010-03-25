\name{getSMfromModel}
\encoding{latin1}
\Rdversion{1.1}
\alias{getSMfromModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get stochiometric matrix from the model file}
\description{ \code{getSMfromModel} takes the model file as the argument and returns the stochiometric matrix from the model file }
\usage{getSMfromModel(modelFile,uptake)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelFile}{ A character string giving the name of model file as generated from the createBiggModel or pruneBiggModel function. }
  \item{uptake}{ A character string giving the name of the name of the uptake substrate. }
}

\value{The value returned is a two dimentional matrix representing the stochiometric matrix }

\author{Anand K. Gavai <anand.gavai@bioinformatics.nl>}

\seealso{
\link[BiGGR:getStoichiometricMatrix]{getStoichiometricMatrix}
}

\examples{
	data("Glycolysis")
	sm<-getSMfromModel(Glycolysis,"R_HEX1")
	sm
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stochiometric matrix}
\keyword{model file}% __ONLY ONE__ keyword per line
