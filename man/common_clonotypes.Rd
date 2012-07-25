\name{common_clonotypes}
\alias{common_clonotypes}

\title{Reports clonotypes common between samples.}

\description{
When given two groups of samples, lists the clonotypes that have been
observed at least in one library of each group.  The returned list can
be used to subset a data frame produced by \code{\link{clonotype.table}}.

When given a table of clonotypes, produces a matrix in which each cell reports
the number of clonotypes found in a pair of libraries.
}

\usage{common_clonotypes(group1, group2, mode="groups", data=clonotypes)}

\arguments{
  \item{group1}{A character vector containing clonotype library names}
  \item{group2}{A character vector containing clonotype library names}
  \item{mode}{By default, \dQuote{groups} when \dQuote{group1} or \dQuote{group2} is provided, \dQuote{matrix} mode otherwise.}
  \item{data}{A clonotype table where the data is stored.  Default: \dQuote{clonotypes}.}
}

\value{
In \dQuote{groups} mode, a character vector of clontoype names.  Their order
follows the original row name order of the clonotype table.

In \dQuote{matric} mode, a symmetric matrix of numbers, with the same row and
column names as the row names of the clonotype table used.
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

# Matrix of numbers of common clonotypes
common_clonotypes()
}

\keyword{ clonotype }% __ONLY ONE__ keyword per line
