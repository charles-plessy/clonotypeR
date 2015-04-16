\name{extdata}
\docType{data}
\alias{codon_ids}
\alias{V_after_C}
\alias{J_before_FGxG}
\title{Extra data used to calculate ID numbers in Yassai et al.'s nomenclature.}
\description{
Data frame derived from Table 1 of Yassai et al., 2009, to construct clonotype names.
}

\details{
\code{V_after_C}: sequence of the V segments after their conserved cystein.

\code{J_before_FGxG}: sequence of the J segments before their conserved FGxG motif.

\code{codon_ids}: data frame derived from Table 1 of Yassai et al., 2009, to construct clonotype names.

The V_after_C and J_before_FGxG tables are, generated from the mouse reference data with the
command: \code{make refresh-data} in the source repository of clonotypeR.
}

\value{

codon_ids: 

\item{codon}{Nucleotide triplet.}
\item{aminoacid}{Single-letter amino acid abbreviation (\dQuote{O} for stop).}
\item{id}{ID numbers assigned to the codons for each amino acids.}

J_before_FGxG:

\item{row name}{J segment name, for instance \dQuote{TRAJ61}.}
\item{sequence}{Sequence of the nucleic acids preceding the first codon of the conserved FGxG motif.}

V_after_C:

\item{row name}{V segment name, for instance \dQuote{TRAV1}.}
\item{sequence}{Sequence of the nucleic acids following the codon of the conserved cysteine.}

}

\examples{
V_after_C <- read.table(system.file('extdata', 'V_after_C.txt.gz', package = "clonotypeR"), stringsAsFactors=FALSE)

J_before_FGxG <- read.table(system.file('extdata', 'J_before_FGxG.txt.gz', package = "clonotypeR"), stringsAsFactors=FALSE)

codon_ids <- read.table(system.file('extdata', 'codon_ids.txt.gz', package = "clonotypeR"), header=TRUE, row.names=1)
}

\references{
A clonotype nomenclature for T cell receptors.
Maryam B. Yassai, Yuri N. Naumov, Elena N. Naumova and Jack Gorski
Immunogenetics, 2009, Volume 61, Number 7, Pages 493-502
}

\seealso{
\code{\link{yassai_identifier}}
}

\keyword{ clonotype }
\keyword{ nomenclature }
\keyword{ Yassai }
