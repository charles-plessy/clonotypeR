\name{read.clonotypes}
\alias{read.clonotypes}

\title{
Reads a clonotype table and returns a data frame.
}

\description{
Reads a clonotype table and returns a data frame.  The clonotype table
is in a TAB-separated format, and the returned data frame has eight
columns: lib, V, J, read, dna, qual, pep, improductive for library name, V and J
segments names, sequence read identifier, DNA, sequence quality and
aminoacid sequence of the CDR3 region, and mark for improductive recombinations respectively
}

\usage{
read.clonotypes("File Name", ...)
}

\arguments{
  \item{"File Name"}{
Path to the tabulation-delimited text file containing the extracted clonotypes.
}
  \item{...}{
The rest of the arguments are passed to the read.table() function
}
}

\details{
Clonotypes are marked improductive if the length of their DNA sequence is not a
multiple of 3, or if it contains a stop codon.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Charles Plessy}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{clonotype.table}},
\code{\link{read.table}}
}

\examples{
cdr3 <- read.clonotypes(system.file("clonotypes.txt"))
}

\keyword{clonotypes}
