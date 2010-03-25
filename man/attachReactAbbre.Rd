\name{attachReactAbbre}
\encoding{latin1}
\Rdversion{1.1}
\alias{attachReactAbbre}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Attaches abbreviation prefix "R_" for the reaction identifiers}
\description{ Attaches abbreviation prefix "R_" (Reactions) }
\usage{ attachReactAbbre(Reactions) }
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{Reactions}{ This file is exported from database of metabolic reconstructions (BiGG) at \url{http://bigg.ucsd.edu/bigg/main.pl} }
}

\value{
	 A character vector with reaction names from reaction file \code{Reactions} with a prefix "R_" attached
}
\references{
http://bigg.ucsd.edu/
}
\author{ Anand K. Gavai <anand.gavai@bioinformatics.nl> }


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{
	## The function is used to get abbreviation for the 
	## reaction names from the reactions file
	abbre<- attachReactAbbre("/examples/Metabolites.txt")
	abbre
}
}


