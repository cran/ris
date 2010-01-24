\name{ris.citation}
\alias{ris.citation}

\title{Citing R and R Packages}

\description{
Hot to cite R and R packages in publications. Wrapper for \code{\link{citation}} that allows for reading from CITATION.RData containing package reference as output from \code{\link{read.ris}}
}

\usage{
ris.citation(package = "base", lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{ character string with the name of a single package. An error occurs if more than one package name is given.}
  \item{lib.loc}{a character vector with path names of R libraries, or NULL. The default value of NULL corresponds to all libraries currently known. If the default is used, the loaded packages are searched before the libraries.}
}

\details{
If there is no \file{CITATION} file, function looks for \file{CITATION.RData} in pkg/data. Expects that \file{CITATION.RData} contains list object, names CITATION, which is the output from \code{\link{read.ris}}. This wrapper is intended to allow for the easy transfer of an RIS file containing the package citations to the form of the citations as they are appear in publications for the \code{R} user. 

Name of data file in pkg/data must be named \file{CITATION.RData} and list output from \code{read.ris} must be named \code{CITATION}.
}

\value{
Display of text citation and BibTeX entry.
}

\author{S. Kovalchik \email{skoval@ucla.edu}}

\seealso{\code{\link{citation}}}

\examples{
\dontrun{
 #SUPPOSE WE HAVE THE RIS CITATIONS FOR BOOT PACKAGE IN WORKING DIRECTORY
CITATION <- read.ris(file="/boot.ris")

 #CREATE CITATION OBJECT AND SAVE AS CITATION.RDATA FOR "PKG"
save(CITATION,file="~/pkg/data/CITATION.RData")

 #AFTER PACKAGE INSTALLATION
ris.citation("boot")
 }
}

\keyword{misc}
