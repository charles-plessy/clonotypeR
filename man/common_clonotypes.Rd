\name{common_clonotypes}
\alias{common_clonotypes}

\title{Lists clonotypes common to two groups of samples.}

\description{
Considering two groups of samples, lists the clonotypes that have been
observed at least in one library of each group.  The returned list can
be used to subset a data frame produced by
\code{\link{clonotype.table}}.
}

\usage{common_clonotypes(group1, group2, data=clonotypes)}

\arguments{
  \item{group1}{A character vector containing clonotype library names}
  \item{group2}{A character vector containing clonotype library names}
  \item{data}{A clonotype table where the data is stored.  Default: \dQuote{clonotypes}.}
}

\value{
Character vector of clontoype names.  Their order follows the original row name
order of the clonotype table.
}

\author{Charles Plessy}

\seealso{
\code{\link{clonotype.table}}, \code{\link{unique_clonotypes}}
}

\examples{
# Load example data
clonotypes.long <- read.clonotypes(system.file("data/clonotypes.txt.gz", package = "clonotypeR"))
clonotypes <- clonotype.table(levels(clonotypes.long$lib), from=clonotypes.long)
summary(clonotypes)

# List clonotypes found in library A, and B or C.
common_clonotypes(group1="A", group2=c("B","C"))
}

\keyword{ clonotype }% __ONLY ONE__ keyword per line
