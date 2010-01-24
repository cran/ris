\name{write.bibtex}
\alias{write.bibtex}

\title{BibTeX Output of List of Citation Objects}

\description{
Writes BibTeX-formatted file given list of citation objects
}

\usage{
write.bibtex(citations,file,append=TRUE)
}

\arguments{
  \item{citations}{List of citation objects, output from \code{citentry} or \code{ris2citentry}}
  \item{file}{character of file to be created (should have extension .bib)}
  \item{append}{Logical, should file be created or append to existing? Default is \code{TRUE}.}
}

\value{
No return object
}

\author{S. Kovalchik \email{skoval@ucla.edu}}

\seealso{\code{\link{toBibtex}}}

\examples{
\dontrun{
data(pearson)
pearson.citation <- ris2citentry(pearson[1:10])

 #CREATES BIB FILE IN WORKING DIRECTORY OF FIRST 10 CITATIONS

write.bibtex(pearson.citation,file="pearson.bib",append=FALSE)
 }
}


\keyword{print}
\keyword{file}

