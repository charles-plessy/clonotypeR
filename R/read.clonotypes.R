read.clonotypes <- function (filename, ...) {

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
	...
)

improductive_length     <- nchar(cdr$dna) %% 3 > 0
improductive_stop       <- grepl ('\\*', cdr$pep)

cdr$improductive        <- improductive_length | improductive_stop

return(cdr)

}
