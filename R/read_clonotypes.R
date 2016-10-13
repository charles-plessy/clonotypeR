#' read_clonotypes
#' 
#' Reads a clonotype_table and returns a data frame.
#' 
#' Reads a clonotype_table in a TAB-separated or OSCT format, and returns a data
#' frame that has eight columns, for library name, V and J segments names,
#' sequence read identifier, DNA, sequence quality, aminoacid sequence of the
#' CDR3 region, mark for unproductive recombinations, and mark for ambiguous
#' sequences.
#' 
#' @param filename Path to the tabulation-delimited text file containing the
#'        extracted clonotypes.
#' @param scores Set to false to load legacy data that did not contain
#'        \dQuote{score} and \dQuote{mapq} columns.
#' @param ... The rest of the arguments are passed to the \code{read.table()}
#'        function.
#'        
#' @return
#' \item{lib}{Library name (factor).}
#' \item{V}{V segment name (factor).}
#' \item{J}{J segment name (factor).}
#' \item{score}{Alignment score (numeric).}
#' \item{mapq}{Mapping quality (numeric).  A sequence with a good alignment
#'             score will still have a low mapping quality if there are good
#'             alternative alignments to other V segments.}
#' \item{read}{Sequence read identifier (character).}
#' \item{dna}{DNA sequence of the CDR3 region (character).}
#' \item{qual}{Quality values for the DNA sequence (character).}
#' \item{pep}{Translation of the DNA sequence (character).}
#' \item{unproductive}{Flag indicating stop codons or frame shifts (logical).}
#' \item{ambiguous}{Flag indicating that the DNA sequences has ambiguous
#'                  (\dQuote{N}) nucleotides (logical).}
#'                  
#' @seealso 
#' \code{\link{clonotype_table}}, \code{\link{is_unproductive}},
#' \code{\link{read.table}},
#' Order Switchable Column Table (OSCT, \url{http://sourceforge.net/projects/osctf/})
#' 
#' @importFrom utils read.table
#'  
#' @examples 
#' 
#' clonotypes <- read_clonotypes(system.file( 'extdata', 'clonotypes.txt.gz'
#'                                          , package = "clonotypeR"))
#' 
#' @export

read_clonotypes <- function (filename, scores=TRUE, ...) {

# Inspect the file to see how many header lines it contains.
# Because of the sequence quality scores, comment.char can not be set.
# This currently skips the header entirely, but in a later version,
# this function could make use of a generic OSCTable parser package.

if (filename == "") stop ("Empty file name.")

cdr.file <- file(filename)
open(cdr.file)
cur_line <- readLines(cdr.file, n=1)
lines_to_skip <- 0
while( grepl("^#", cur_line) ) {
  cur_line <- readLines(cdr.file, n=1)
  lines_to_skip <- lines_to_skip + 1
}
close(cdr.file)

# If it has lines to skip, then it is an OSCTable file, and therefore it has a header.
if (lines_to_skip ==  0) {
	has_header <- FALSE
} else {
	has_header <- TRUE
}

if (scores == TRUE) {
  cdr.col.names <- c( "lib",     "V",      "J",      "score",   "mapq",    "read",      "dna",       "qual",      "pep"      )
  cdr.col.class <- c( "factor",  "factor", "factor", "numeric", "numeric", "character", "character", "character", "character")
} else {
  cdr.col.names <- c( "lib",     "V",      "J",                            "read",      "dna",       "qual",      "pep"      )
  cdr.col.class <- c( "factor",  "factor", "factor",                       "character", "character", "character", "character")
}

# Load the clonotypes in a data frame.

cdr <- read.table(
	filename,
	col.names	=	cdr.col.names,
	colClasses	=	cdr.col.class,
	comment.char	=	'',
	quote		=	'',
	sep		=	'\t',
	header		=	has_header,
	skip		=	lines_to_skip,
	...
)

# Add a new column marking unproductive recombinations.

cdr$unproductive        <- is_unproductive(cdr)

cdr$ambiguous           <- grepl('N', cdr$dna)

return(cdr)
}
