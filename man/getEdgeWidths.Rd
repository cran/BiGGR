\name{getEdgeWidths}
\encoding{latin1}
\Rdversion{1.1}
\alias{getEdgeWidths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getEdgeWidths}
\description{gets the descritized value for rates (fluxes) of reactioins to be displayed on the graph object}
\usage{getEdgeWidths(rates, levels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rates}{is a one dimentional numeric vector with values representing fluxes as computed from the \code{LIM} package from the model file}
  \item{levels}{a numeric value representing the levels at which the fluxes need to be descritized}
}

\value{
 \item{dz_rates}{a one dimentional numeric vector of descritized rates for individual reactions}
}

\author{Anand K. Gavai <anand.gavai@bioinformatics.nl>}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getRates},\link{createBiggModel}}
}

\examples{
 data("Glycolysis")
 levels<-7
 rates<-getRates(Glycolysis)
 dz_rates<-getEdgeWidths(rates,levels) 
 dz_rates
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{flux weights}

