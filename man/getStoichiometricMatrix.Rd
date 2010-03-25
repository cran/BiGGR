\name{getStoichiometricMatrix}
\encoding{latin1}
\Rdversion{1.1}
\alias{getStoichiometricMatrix}
\title{Get entire stoichiometric matrix from the metabolic reconstruction for a specific organism exported in SBML format}
\description{get stoichiometric matrix from the model(sbmlobj), where sbmlobj is of type \code{SBML}}
\usage{getStoichiometricMatrix(sbmlobj)}

%- maybe also 'usage' for other sbmlobjs documented here.
\arguments{
  \item{sbmlobj}{ is of class \code{SBML}. }
}

\value{ \code{sm} returns a matrix of the entire reconstruction for an organizm(s) available from the BiGG Database}


\author{Anand K. Gavai <anand.gavai@bioinformatics.nl>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\link[BiGGR:getAdjMfromStoM]{getAdjMfromStoM}
}
\examples{
data(H.sapiens_Recon_1)
	sm<-getStoichiometricMatrix(model(H.sapiens_Recon_1))
	sm
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stoichiometric matrix}
