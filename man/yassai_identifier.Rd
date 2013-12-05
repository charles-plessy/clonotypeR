\name{yassai_identifier}
\alias{yassai_identifier}
\alias{yassai_identifier,ANY,missing,missing,ANY-method}
\alias{yassai_identifier,character,data.frame,data.frame,ANY-method}
\alias{yassai_identifier,data.frame,data.frame,data.frame,logical-method}

\title{TCR clonotype identifier (Yassai et al.).}
\description{

The clonotype nomenclature defined by Yassai et al.
in \url{http://dx.doi.org/10.1007/s00251-009-0383-x}.

}

\usage{yassai_identifier(data, V_after_C, J_before_FGxG, long=FALSE)}

\arguments{
  \item{data}{A data frame or a character vector containing a clonotype(s) with proper row or element names.}
  \item{V_after_C}{(optional) A data frame indicating the aminoacids following the conserved cystein for each V segment.}
  \item{J_before_FGxG}{(optional) A data frame indicating the aminoacids preceding the conserved FGxG motif for each V segment.}
  \item{long}{(optional) Display the codons that would be trimmed following the standard nomenclature.}
}

\details{
  By default, \code{yassai_identifier()} assume mouse sequences and will load
  the V_after_C and J_before_FGxG tables distributed in this package.  It is
  possible to provide alternative tables either by passing them directly as
  argument, or by installing them as \dQuote{./inst/extdata/V_after_C.txt.gz} and
  \dQuote{./inst/extdata/J_before_FGxG.txt.gz}.

  The \sQuote{long} mode is needed when two colonotypes or more give the same
  identifier.  See below for an example.
}

\value{
The name (for instance sIRSSy.1456B19S1B27L11) consists of five segments:
  \enumerate{
    \item CDR3 amino acid identifier (ex. sIRSSy), followed by a dot ;
    \item CDR3 nucleotide sequence identifier (ex. 1456) ;
    \item variable (V) segment identifier (ex. BV19S1) ;
    \item joining (J) segment identifier (ex. BJ2S7) ;
    \item CDR3 length identifier (ex. L11).
  }
}

\author{Charles Plessy}


\seealso{
\code{\link{codon_ids}}, \code{\link{J_before_FGxG}}, \code{\link{V_after_C}}
}

\examples{
clonotypes <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
head(yassai_identifier(clonotypes))

# The following two clonotypes have a the same identifier, and are
# disambiguated by using the long mode

yassai_identifier(c(V="TRAV14-1", J="TRAJ43", dna="GCAGCTAATAACAACAATGCCCCACGA", pep="AANNNNAPR"))
# [1] "aAn.1A14-1A43L9"

yassai_identifier(c(V="TRAV14-1", J="TRAJ43", dna="GCAGCAGCTAACAACAATGCCCCACGA", pep="AAANNNAPR"))
# [1] "aAn.1A14-1A43L9"

yassai_identifier(c(V="TRAV14-1", J="TRAJ43", dna="GCAGCTAATAACAACAATGCCCCACGA", pep="AANNNNAPR"), long=TRUE)
# [1] "aAnnnnapr.1A14-1A43L9"

yassai_identifier(c(V="TRAV14-1", J="TRAJ43", dna="GCAGCAGCTAACAACAATGCCCCACGA", pep="AAANNNAPR"), long=TRUE)
# [1] "aaAnnnapr.1A14-1A43L9"
}


\keyword{ Yassai }% __ONLY ONE__ keyword per line
\keyword{ clonotype }% __ONLY ONE__ keyword per line
\keyword{ nomenclature }% __ONLY ONE__ keyword per line
