\name{yassai.nomenclature}
\alias{yassai.nomenclature}

\title{TCR clonotype nomenclature (Yassai et al.).}
\description{

The clonotype nomenclature defined by Yassai et al.
in DOI 10.1007/s00251-009-0383-x.

}

\usage{yassai.nomenclature(clonotype)}

\arguments{
  \item{clonotype}{
A character vector containing a clonotype.
}
}

\details{

The name consists of five segments:

(1) CDR3 amino acid identifier,

(2) CDR3 nucleotide sequence identifier,

(3) variable (V) segment identifier,

(4) joining (J) segment identifier,

(5) CDR3 length identifier.

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
\link{data-clonotypes}
}
\examples{

# Should return: qETRGg.226302-121L13
#yassai.nomenclature(c("C", "TRBV2", "TRBJ1-2", "GINGMWQ04IYGTQ", "GCCAGCAGCCAAGAGACCAGGGGAGGCTCCGACTACACC", "IIIIIIIIIIIIIIIIIIIBBBBIFFI>>>IIIIIIIHH", "ASSQETRGGSDYT"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ ~clonotype }% __ONLY ONE__ keyword per line
\keyword{ ~nomenclature }% __ONLY ONE__ keyword per line
