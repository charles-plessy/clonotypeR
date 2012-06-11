\name{unique.clonotypes}
\alias{unique.clonotypes}

\title{lists unique clonotypes in libraries.}

\description{
Finds all the clonotypes expressed in one or more libraries, and returns a
vector where they are listed once.  This vector can be used to subset a
clonotype table.
}

\usage{unique.clonotypes(libs, from=clonotypes)}

\arguments{
  \item{libs}{A character vector contain clonotype library names}
  \item{from}{The name of the clonotype table where the data is stored.}
}

\details{
Uses the table called "clonotypes" by default.
}

\value{
Character vector of clontoype names.  Their order follows the original row name
order of the clonotype table.
}

\author{Charles Plessy}


\seealso{
\code{\link{clonotype.table}}, ~~~
}

\examples{
# Load example data
clonotypes.long <- read.clonotypes(system.file("data/clonotypes.txt", package = "clonotypeR"))
clonotypes <- clonotype.table(levels(clonotypes.long$lib), from=clonotypes.long)
summary(clonotypes)

# List clonotypes found in library A.
unique.clonotypes("A")

# List clonotypes found in library A or B.
unique.clonotypes(c("A","B"))
}

\keyword{ ~clonotype }% __ONLY ONE__ keyword per line
