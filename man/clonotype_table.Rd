\name{clonotype_table}
\alias{clonotype_table}
\title{Create a table count of clonotypes or other features.
}
\description{
Using a clonotype data frame loaded with \code{\link{read_clonotypes}},
\code{clonotype_table} will create a table counting how many times each
clonotypes have been seen in each libraries.  By default, the unproductive
rearrangements are filtered out.
}

\usage{
clonotype_table(libs, feats=c("V","pep","J"), data, filter=data$unproductive, minscore=0, minqual=1)
}

\arguments{
  \item{libs}{
  A character vector containing the name of one or many libraries.  Same names
  must not appear twice.  If no library names are provided, all the libraries
  present in the clonotypes data frame will be used.
}
  \item{feats}{
  What to count.  By default, it counts clonotypes, defined as \code{c("V","pep","J")}.
  But it can also count single features, such as the V or J segments.
}
  \item{data}{
  Data frame as loaded by \code{\link{read_clonotypes}}.
}
  \item{filter}{
  Logical vector to filter out clonotypes.  By default it relies on the clonotypes
  data frame to provide a \dQuote{unproductive} column, that indicates clonotypes with a
  stop codon or a frame shift.
}
  \item{minscore}{
  Minimum alignment score.  Clonotypes with an alignment score lower than this value are
  discarded.
}
  \item{minqual}{
  Minimum mapping quality.  Clonotypes with a mapping quality lower than this value are
  discarded.
}
}

\value{
\code{\link{clonotype_table}} returns a data frame, where row names are features
(clonotypes, segment names, \dots), column names are libraries, and values are number
of times each feature was found in each library.
}

\author{Charles Plessy}

\seealso{
\code{\link{read_clonotypes}}
}

\examples{
# Read the package's example data
clonotypes <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))

# Inspect the alignment scores
hist(clonotypes$score)

# Count J segments
j <- clonotype_table(levels(clonotypes$lib), "J", data=clonotypes)

# Normalise counts in parts per million
J <- data.frame(prop.table(as.matrix(j),2) * 1000000)
}
\keyword{ clonotype }
