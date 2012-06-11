\name{Sample clonotypes}
\docType{data}
\alias{clonotypes}
\title{
Sample clonotype data.
}
\description{
Sample TCR clonotypes extracted from 454 reads and stored as a TAB-separated
plain text file, to be loaded with the read.clonotypes() function.
}
\usage{
data(clonotypes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
The rest of the arguments are passed to teh read.table() function
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
CDR3_COLNAMES  <- c(
	"lib",
	"V",
	"J",
	"read",
	"dna",
	"qual",
	"pep"
)

CDR3_CLASSES <- c(
	"factor",
	"factor",
	"factor",
	"character",
	"character",
	"character",
	"character"
\item{lib}{factor}
\item{V}{factor}
\item{J}{factor}
\item{read}{character}
\item{dna}{character}
\item{qual}{character}
\item{pep}{character}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
read.table()
}
\examples{
clonotypes <- read.clonotypes(system.file("data/clonotypes.txt", package = "clonotypeR"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
