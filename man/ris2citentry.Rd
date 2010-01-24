\name{ris2citentry}
\alias{ris2citentry}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Convert Reference List to Citation Objects}

\description{
Creates citation object, using \code{\link{citEntry}} from output of \code{\link{read.ris}}
}

\usage{
ris2citentry(citation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{citation}{List of references in \code{read.ris} output format}
}

\details{
textVersion is constructed according to entry type and ASA-style citation. Only fields for BibTeX entries are included in citation object. Arguments \code{header} and \code{footer} are left \code{NULL}.
}

\value{
List of citation objects
}


\author{S. Kovalchik \email{skoval@ucla.edu}}

\examples{
data(pearson)
ris2citentry(pearson[1:3])
}

\keyword{misc}











