\name{unique_clonotypes}
\alias{unique_clonotypes}

\title{Lists unique clonotypes in libraries.}

\description{
Finds all the clonotypes expressed in one or more libraries, and returns a
vector where they are listed once.  This vector can be used to subset a
clonotype table.
}

\usage{unique_clonotypes(libs, data)}

\arguments{
  \item{libs}{A character vector contain clonotype library names}
  \item{data}{The name of the clonotype table where the data is stored.}
}

\details{
Uses the table called \dQuote{clonotypes} by default.
}

\value{
Character vector of clonotype names.  Their order follows the original row name
order of the clonotype table.
}

\author{Charles Plessy}


\seealso{
\code{\link{clonotype.table}}, \code{\link{common_clonotypes}}
}

\examples{
# Load example data
clonotypes.long <- read.clonotypes(system.file("data/clonotypes.txt.gz", package = "clonotypeR"))
clonotypes <- clonotype.table(levels(clonotypes.long$lib), data=clonotypes.long)
summary(clonotypes)

# List clonotypes found in library A.
unique_clonotypes("A", data=clonotypes)

# List clonotypes found in library A or B.
unique_clonotypes(c("A","B"), data=clonotypes)
}

\keyword{ clonotype }% __ONLY ONE__ keyword per line
