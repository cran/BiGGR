\name{possibleExternals}
\encoding{latin1}
\Rdversion{1.1}
\alias{possibleExternals}

\title{possibleExternals}

\description{Finds the possible externals from an object of type \code{liminput}}

\usage{possibleExternals(liminput)}

\arguments{
  \item{liminput}{is an object of type liminput}
}


\references{Soetaert K, van Oevelen D (2009). LIM: Linear Inverse Model examples and solution methods.
R package version 1.3}
\author{Anand K. Gavai <anand.gavai@bioinformatics.nl> }
\note{none}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
	
  \dontrun{
	data("Glycolysis")
	liminput<-Read(Glycolysis)
	ext<-possibleExternals(liminput)
	ext
	}
  }
\keyword{Linear Inverse Models}

