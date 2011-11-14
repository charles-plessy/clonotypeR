\name{unique.clonotypes}
\alias{unique.clonotypes}

\title{lists unique clonotypes in libraries.}
\description{

This function finds all the clonotypes expressed in one or more
libraries, and returns a character vector where they are listed
once. By default, it will use the clontype table called clonotype,
where the row names are clonotypes. The order of the list returned by
unique.clonotypes is the original row name order.

}

\usage{unique.clonotypes(libs, using=clonotypes)}

\arguments{
  \item{libs}{
A character vector contain clonotype library names
}
  \item{using}{
The name of the clonotype table where the data is stored.
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

\author{Charles Plessy}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# Load example data
clonotypes.long <- read.clonotypes(system.file("clonotypes.txt"))
clonotypes <- clonotypes.table(clonotypes.long)
summary(clonotypes)

# List clonotypes found in library A.
unique.clonotypes("A")

# List clonotypes found in library A or B.
unique.clonotypes(c("A","B"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ ~clonotype }% __ONLY ONE__ keyword per line
