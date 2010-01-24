\name{read.ris}
\alias{read.ris}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Import Bibliographic Data}

\description{
Reads a file in Research Information Systems (RIS) format and returns a list with named BibTeX-like fields
}

\usage{
read.ris(file, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{character of RIS filename}
  \item{\dots}{other arguments to \code{\link{readLines}}}
}

\details{

The reference types are assigned according to the following RIS types:
\itemize{
\item article: ABST, INPR, JFULL, JOUR, MGZN or NEWS
\item book: SER or BOOK
\item incollection: CONF or CHAP
\item phdthesis: THES
\item misc: RPRT, UNPB or anything else
}
}

\value{
A list with the following named tags

\tabular{lll}{
 \bold{Tag} \tab \bold{Value description} \tab \bold{RIS field} \cr
 \code{ris.type} \tab RIS description of citation type \tab \cr
 \code{type} \tab One of article, book, incollection, phdthesis or misc
 \tab  TY corresponding entries; see Details \cr
 \code{id} \tab \tab ID \cr
\code{title.primary} \tab Source title \tab T1, TI, BT, CT \cr
 \code{title.secondary} \tab Title of collection/book/series for source in collection \tab T2, T3 \cr
\code{authors.primary} \tab Character vector of first authors with string entries of "last, first middle" \tab A1, AU \cr
 \code{authors.secondary} \tab Editors, translators, etc. \tab A2, A3, ED \cr
 \code{keywords} \tab character vector, each element is a keyword \tab KW \cr
 \code{date} \tab List with components year, month, date, info \tab Y1, PY \cr
 \code{year} \tab Numeric publication year taken from data \tab \cr
 \code{abstract} \tab Notes or abstract \tab N1, N2, AB \cr
 \code{journal} \tab \tab JF, JO \cr
\code{journal.abbrev} \tab Index Medicus style abbreviation \tab JA \cr
\code{volume} \tab \tab VL\cr
 \code{number} \tab \tab IP, CP\cr
 \code{city} \tab \tab CY \cr
 \code{publisher} \tab \tab PB\cr
 \code{isbn} \tab ISSN/ISBN \tab SN\cr
 \code{address} \tab \tab AD\cr
 \code{pages} \tab character vector with first page and last page\tab SP, EP \cr
 \code{url} \tab Source URL (i.e. JSTOR) \tab UR\cr
 }
}

\author{S. Kovalchik \email{skoval@ucla.edu}}

\seealso{\code{\link{citEntry}}; For information on RIS formatting \url{http://www.refman.com}}

\examples{

##Journal article lifetime bibliography of Karl Pearson
pearson.bib <- read.ris(file="http://skoval.bol.ucla.edu/pearson.ris")

##Find the first publication
years <- sapply(pearson.bib,function(ref){ref$date$year})
first.year <- which(years==min(years))

##Display articles in the earliest year K. Pearson begins to publish
pearson.bib[first.year]
}

\keyword{file}
\keyword{connection}
