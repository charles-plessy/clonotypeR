\name{clonotypes}
\docType{data}
\alias{clonotypes}
\title{Sample clonotype data.}

\description{
Sample TCR clonotypes extracted from 454 reads and stored as a TAB-separated
file, to be loaded with the \code{\link{read.clonotypes}} function.
}

\usage{
data(clonotypes)
}

%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{
\item{lib}{Library name (factor).}
\item{V}{V segment name (factor).}
\item{J}{J segment name (factor).}
\item{read}{Sequence read identifier (character).}
\item{dna}{DNA sequence of the CDR3 region (character).}
\item{qual}{Quality values for the DNA sequence (character).}
\item{pep}{Translation of the DNA sequence (character).}
}

\seealso{
\code{\link{read.clonotypes}}
}

\keyword{ clonotypes }
\keyword{ 454 }
