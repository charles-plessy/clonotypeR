\name{Codon IDs}
\docType{data}
\title{Codon ID numbers in Yassai et al.'s nomenclature.}
\description{
From Table 1 of Yassai et al., 2009, to construct clonotype names.
}
\usage{
data(codon-ids)
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
\item{codon}{character}
\item{aminoacid}{character}
\item{id}{character}
}
\references{
Yassai et al., 2009
}
\author{
Yassai et al., 2009
}

\seealso{
\code{\link{yassai.nomenclature}}
}
\examples{
%cdr3 <- read.clonotypes(system.file("clonotypes.txt"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~clonotype }
\keyword{ ~nomenclature }
\keyword{ ~yassai }
