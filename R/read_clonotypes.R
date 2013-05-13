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

return(cdr)
}
