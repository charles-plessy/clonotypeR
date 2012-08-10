\name{clonotype.table}
\alias{clonotype.table}
\title{Create a table count of clonotypes or other features.
}
\description{
Using a clonotype data frame loaded with \code{\link{read.clonotypes}},
\code{clonotype.table} will create a table counting how many times each
clonotypes have been seen in each libraries.  By default, the improductive
rearrangements are filtered out.
}

\usage{
clonotype.table(libs, feats=c("V","pep","J"), from=clonotypes, filter=from$improductive, minscore=0, minqual=1)
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
  \item{from}{
  Data frame as loaded by \code{\link{read.clonotypes}}.  By default,
  \code{clonotype.table} will use a data frame called \sQuote{clonotypes}.
}
  \item{filter}{
  Logical vector to filter out clonotypes.  By default it relies on the clonotypes
  data frame to provide a \dQuote{improductive} column, that indicates clonotypes with a
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
\code{\link{clonotype.table}} returns a data frame, where row names are features
(clonotypes, segment names, \dots), column names are libraries, and values are number
of times each feature was found in each library.
}

\author{Charles Plessy}

\seealso{
\code{\link{read.clonotypes}}
}

\examples{
# Read the package's example data
clonotypes <- read.clonotypes(system.file("data/clonotypes.txt.gz", package = "clonotypeR"))

# Inspect the alignment scores
\dontrun{hist(clonotypes$score)}

# Count J segments
j <- clonotype.table(levels(clonotypes$lib), "J")

# Normalise counts in parts per million
J <- data.frame(prop.table(as.matrix(j),2) * 1000000)
}
\keyword{ clonotype }
