\name{attachMetAbbre}
\encoding{latin1}
\Rdversion{1.1}
\alias{attachMetAbbre}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Attaches abbreviation prefix "M_" for the metabolite identifiers}
\description{ Attaches abbreviation prefix "M_" (Reactions) }
\usage{ attachMetAbbre(Metabolites) }
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{Metabolites}{ This file is exported from database of metabolic reconstructions (BiGG) at \url{http://bigg.ucsd.edu/bigg/main.pl} }
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
	abbre<- attachMetAbbre("/examples/Metabolites.txt")
	abbre
}
}


