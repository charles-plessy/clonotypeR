read.clonotypes <- function (filename, ...) {

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

# Load the clonotypes in a data frame.

cdr <- read.table(
	filename,
	col.names	=	c(
					"lib",
					"V",
					"J",
					"read",
					"dna",
					"qual",
					"pep"
				),
	colClasses	=	c(
					"factor",
					"factor",
					"factor",
					"character",
					"character",
					"character",
					"character"
				),
	comment.ch	=	'',
	quote		=	'',
	sep		=	'\t',
	header		=	has_header,
	skip		=	lines_to_skip,
	...
)

# Add a new column marking improductive recombinations.

improductive_length     <- nchar(cdr$dna) %% 3 > 0
improductive_stop       <- grepl ('\\*', cdr$pep)
cdr$improductive        <- improductive_length | improductive_stop

return(cdr)
}
