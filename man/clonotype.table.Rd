\name{clonotype.table}
\alias{clonotype.table}
\title{
Create a table count of clonotypes or other features.
}
\description{
Using a clonotype data frame loaded with \code{\link{read.clonotypes}},
\code{clonotype.table} will create a table counting how many times each
clonotypes have been seen in each libraries.
}
\usage{
clonotype.table(libs, feats=c("V","pep","J"), from=clonotypes, filter=from$improductive)
}

\arguments{
  \item{libs}{
  A character vector containing the name of one or many libraries.  Same names
  must not appear twice.
}
  \item{feats}{
  What to count.  By default, it counts clonotypes, defined as c("V","pep","J").
  But it can also count single features, such as the V or J segments.
}
  \item{from}{
  This is a data frame as loaded by \code{\link{read.clonotypes}}.  By default,
  \code{clonotype.table} will use a data frame called clonotypes.
}
  \item{filter}{
  Boolean vector to filter out clonotypes.  By default it relies on the clonotypes
  data frame to provide a "improductive" column, that indicates clonotypes with a
  stop codon or a frame shift.
}
}

\details{
  By default, the improductive rearrangements are ignored, but there is a hidden
  option called filter that can be used to modify this behavior.

  These tables can be normalised in parts per million with a command like
  apply(data-frame-to-normalise, 2, function (X) { 1000000 * X / sum(X, na.rm = T) }
}
\value{
\code{\link{read.clonotypes}} returns a data frame, where row names are features
(segment names, clonotypes, \dots), column names are libraries, and values are number
of times each feature was found in each library.
}

\author{Charles Plessy}

\seealso{
\code{\link{read.clonotypes}}
}
\examples{
clonotypes <- read.clonotypes(system.file("data/clonotypes.txt", package = "clonotypeR"))
j <- clonotype.table(levels(clonotypes$lib), "J")
J <- apply(j, 2, function (X) { 1000000 * X / sum(X, na.rm = TRUE) })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ clonotype }
