\name{yassai.identifier}
\alias{yassai.identifier}

\title{TCR clonotype identifier (Yassai et al.).}
\description{

The clonotype nomenclature defined by Yassai et al.
in \url{http://dx.doi.org/10.1007/s00251-009-0383-x}.

}

\usage{yassai.identifier(data)}

\arguments{
  \item{data}{A data frame or a character vector containing a clonotype(s) with proper row or element names.}
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
\code{\link{clonotypes}}, \code{\link{codon_ids}}, \code{\link{J_before_FGxG}}, \code{\link{V_after_C}}
}
\examples{
data(clonotypes)
head(yassai.identifier(clonotypes))
## [1] "aAYt.22A14N2A49L10"     "aYt.2A14N2A49L10"       "aGRt.34A14N1A40L11"    
## [4] "aADTIt.1221A14N1A27L11" "AEMn.411A14N1A23L11"    "sAHy.11A14-2A32L14"    
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ Yassai }% __ONLY ONE__ keyword per line
\keyword{ clonotype }% __ONLY ONE__ keyword per line
\keyword{ nomenclature }% __ONLY ONE__ keyword per line
