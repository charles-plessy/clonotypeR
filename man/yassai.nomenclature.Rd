\name{yassai.nomenclature}
\alias{yassai.nomenclature}

\title{TCR clonotype nomenclature (Yassai et al.).}
\description{

The clonotype nomenclature defined by Yassai et al.
in \url{http://dx.doi.org/10.1007/s00251-009-0383-x}.

}

\usage{yassai.nomenclature(clonotype)}

\arguments{
  \item{clonotype}{A character vector containing a clonotype.}
}

\details{

The name (for instance sIRSSy.1456B19S1B27L11) consists of five segments:
  \enumerate{
    \item CDR3 amino acid identifier (ex. sIRSSy), followed by a dot ;
    \item CDR3 nucleotide sequence identifier (ex. 1456) ;
    \item variable (V) segment identifier (ex. BV19S1) ;
    \item joining (J) segment identifier (ex. BJ2S7) ;
    \item CDR3 length identifier (ex. L11).
  }
}

%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}

\author{Charles Plessy}


\seealso{
\code{\link{clonotypes}}
}
\examples{

# Should return: qETRGg.226302-121L13
clonotype <- c("C", "TRBV2", "TRBJ1-2", "GINGMWQ04IYGTQ",
  "GCCAGCAGCCAAGAGACCAGGGGAGGCTCCGACTACACC",
  "IIIIIIIIIIIIIIIIIIIBBBBIFFI>>>IIIIIIIHH", "ASSQETRGGSDYT")
yassai.nomenclature(clonotype)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ Yassai }% __ONLY ONE__ keyword per line
\keyword{ clonotype }% __ONLY ONE__ keyword per line
\keyword{ nomenclature }% __ONLY ONE__ keyword per line
