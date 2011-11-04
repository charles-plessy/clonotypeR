\name{read.clonotypes}
\alias{read.clonotypes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads a clonotype table and returns a data frame.
}
\description{
Reads a clonotype table and returns a data frame.  The clonotype table
is in a TAB-separated format, and the returned data frame has seven
columns: lib, V, J, read, dna, qual, pep for library name, V and J
segments names, sequence read identifier, DNA, sequence quality and
aminoacid sequence of the CDR3 region respectively
}
\usage{
read.clonotypes("File Name")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
The rest of the arguments are passed to teh read.table() function
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
read.table()
}
\examples{
cdr3 <- read.clonotypes(system.file("clonotypes.txt"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
