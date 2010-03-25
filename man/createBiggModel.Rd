\name{createBiggModel}
\encoding{latin1}
\Rdversion{1.1}
\alias{createBiggModel}

\title{createBiggModel}

\description{creates a model file to be run for simulations of metabolic fluxes}

\usage{createBiggModel(metabolites_file,reactions_file,maximize,
	equation_var,equation_value,constraint,externals)}

\arguments{

  \item{metabolites_file}{is a dataframe of metabolites participating in a reaction. This file is exported from database of metabolic reconstructions (BiGG) at 
				  \url{http://bigg.ucsd.edu/bigg/searchMet.pl} }

  \item{reactions_file}{is a dataframe of reactions participating in a metabolic pathway. This file can be exported from database of metabolic reconstructions (BiGG) at 
				\url{http://bigg.ucsd.edu/bigg/main.pl} }

  \item{maximize}{
	is a character vector consisting the tag of the reaction(s) to be maximized or minimized
}
  \item{equation_var}{
	is a character vector specifying the name for the reaction to be maximized or minimized 
}
  \item{equation_value}{
	is a numeric value for the reactions specified in \code{equation_var}
}
  \item{constraint}{
   	 is a character vector specifying the minimum and maximum values(boundary) under which the solution for the 	\code{maximize} reaction should fall
}
  \item{externals}{
	a character vector of metabolites as provided by the user for speficific pathways for which FBA (flux balance analysis needs to be performed)
	}
}

\value{A model file with with extension \code{".lim"} is created}

\references{Soetaert K, van Oevelen D (2009). LIM: Linear Inverse Model examples and solution methods.
R package version 1.3}
\author{Anand K. Gavai <anand.gavai@bioinformatics.nl>}
\note{none}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
	## The function is currently defined as

	\dontrun{
	metabolites_file<-"glycolysis_M.csv" # download from the BiGG database
	reactions_file<-"glycolysis_R.csv"   # download from the BiGG database
	maximize<-"R_PYK"
	equation_var<-"R_HEX1"
	equation_value<-1
	constraint<-"[0,1000]"
	externals<-c("glcD","pyr")
	model.lim<-createBiggModel(metabolites_file,reactions_file,maximize,equation_var,equation_value,constraint,externals)

	}

}
\keyword{Linear Inverse Models}
\keyword{Linear optimization model file}

