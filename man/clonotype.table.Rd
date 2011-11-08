\name{clonotype.table}
\alias{clonotype.table}
\title{
Create a count table of clonotypes or subfeatures.
}
\description{
Using a clonotype data frame loaded with \code{\link{read.clonotypes}},
\code{clonotype.table} will create a table counting how many times each
clonotypes have been seen in each libraries.
}
\usage{
clonotype.table(table, FEAT, ...)
}

\arguments{
  \item{table}{
  This is a table as loaded by \code{\link{read.clonotypes}}.  By default,
  \code{clonotype.table} will act on all libraries present in this table
}
  \item{\FEAT}{
     ~~Describe \code{\dots} here~~
}
  \item{\dots}{
     ~~Describe \code{\dots} here~~
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
\author{Charles Plessy}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read.clonotypes}}
}
\examples{
clonotypes <- read.clonotypes(system.file("clonotypes.txt"))
clonotype.table(clonotypes)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ clonotype }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
