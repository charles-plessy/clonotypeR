\name{read.clonotypes}

\title{
Reads a clonotype table and returns a data frame.
}

\description{
Reads a clonotype table is in a TAB-separated format, and returns a data frame
that has eight columns, for library name, V and J segments names, sequence read
identifier, DNA, sequence quality, aminoacid sequence of the CDR3 region, mark
for improductive recombinations.
}

\usage{
read.clonotypes("File Name", ...)
}

\arguments{

\item{"File Name"}
{Path to the tabulation-delimited text file containing the extracted clonotypes.}

\item{...}
{The rest of the arguments are passed to the read.table() function.}
}

\details{
Clonotypes are marked improductive if the length of their DNA sequence is not
a multiple of 3, or if it contains a stop codon.  This column is calculated on
the fly.
}

\value{
\item{lib}{Library name (factor).}
\item{V}{V segment name (factor).}
\item{J}{J segment name (factor).}
\item{read}{Sequence read identifier (character).}
\item{dna}{DNA sequence of the CDR3 region (character).}
\item{qual}{Quality values for the DNA sequence (character).}
\item{pep}{Translation of the DNA sequence (character).}
\item{improductive}{Flag indicating stop codons or frame shifts (logical).}
}

\author{Charles Plessy}

\seealso{
\code{\link{clonotype.table}},
\code{\link{read.table}}
}

\examples{
cdr3 <- read.clonotypes(system.file("clonotypes.txt"))
}

\keyword{clonotypes}
