\name{check.duplicates}
\alias{check.duplicates}

\title{Index of Suspected Duplications}

\description{
Identified possible duplicate references given list returned from \code{\link{read.ris}}
}

\usage{
check.duplicates(references)
}

\arguments{
  \item{references}{List of return object from function \code{read.ris}}
}

\details{
Suspected duplicates are returned if any of the following apply: 1) repeated id, 2) repeated isbn or 3) matching partial title and author list. The partial title is the first 50 characters of all words of 5 or more characters in length.
}

\value{
Index of suspected duplicates in order of matches by id, isbn, author/title. If no matches, zero-length integer is returned.
}

\author{S. Kovalchik \email{skoval@ucla.edu}}


\examples{
data(pearson)

suspects <- check.duplicates(pearson[1:5])

 #SUSPECTED DUE TO MATCHING ISBN

pearson[suspects]
}


\keyword{manip}

