\name{is_unproductive}
\alias{is_unproductive}

\title{
Determines if clonotype sequences are productive.
}

\description{
ClonotypeR identifies V and J segments, isolates the DNA sequence between the conserved
cystein and the FGxG motifs, and translates it.  This functions verifies that this
sequence is in frame and has no stop codon.
}

\usage{
is_unproductive(data)
}

\arguments{
\item{data}{Data frame of clonotype sequences, or character vector
describing a single clonotype, where the DNA sequence is available under the
name \dQuote{dna} and its translation available under the name \dQuote{pep}.}
}

\details{Clonotypes are marked unproductive if the length of their DNA sequence
is not a multiple of 3, or if they contain a stop codon, as indicated by an
asterisk in the translated sequence.}

\value{
  Returns a logical vector, with one value per row in the original data.
}

\author{Charles Plessy}

\seealso{
\code{\link{read_clonotypes}}
}

\examples{
clonotypes <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
is_unproductive(clonotypes)
}

\keyword{clonotypes}
