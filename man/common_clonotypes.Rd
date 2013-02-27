\name{common_clonotypes}
\alias{common_clonotypes}

\title{Reports clonotypes common between samples.}

\description{
When given one group of samples, lists the clonotypes that have been observed
at least in one sample.  The returned list can be used to subset a data frame
produced by \code{\link{clonotype_table}}.

When given two groups of samples, lists the clonotypes that have been observed
at least in one sample of each group.  Groups can contain a single sample, in
which case the returned list is simply the clonotypes found in both samples.

When given a table of clonotypes, produces a matrix in which each cell reports
the number of clonotypes found in each pair of samples.
}

\usage{common_clonotypes(group1, group2, mode="groups", data)}

\arguments{
  \item{group1}{A character vector containing clonotype library names}
  \item{group2}{A character vector containing clonotype library names}
  \item{mode}{By default, \dQuote{groups} when \dQuote{group1} or \dQuote{group2} is provided, \dQuote{matrix} mode otherwise.}
  \item{data}{A clonotype_table where the data is stored.}
}

\value{
In \dQuote{groups} mode, a character vector of clonotype names.  Their order
follows the original row name order of the clonotype_table.

In \dQuote{matrix} mode, a symmetric matrix of numbers, with the same row and
column names as the row names of the clonotype_table used.
}

\author{Charles Plessy}

\seealso{
\code{\link{clonotype_table}}, \code{\link{unique_clonotypes}}
}

\examples{
# Load example data
clonotypes.long <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
clonotypes <- clonotype_table(levels(clonotypes.long$lib), data=clonotypes.long)
summary(clonotypes)

# List clonotypes found in library A, and B or C.
common_clonotypes(group1="A", group2=c("B","C"), data=clonotypes)

# Count clonotypes found in library A, and B or C.
length(common_clonotypes(group1="A", group2=c("B","C"), data=clonotypes))

# Matrix of numbers of common clonotypes
common_clonotypes(data=clonotypes)
}

\keyword{ clonotype }% __ONLY ONE__ keyword per line
