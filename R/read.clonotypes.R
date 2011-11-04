read.clonotypes <- function (FILENAME, ...) {

CDR3_COLNAMES  <- c(
	"lib",
	"V",
	"J",
	"read",
	"dna",
	"qual",
	"pep"
)

CDR3_CLASSES <- c(
	"factor",
	"factor",
	"factor",
	"character",
	"character",
	"character",
	"character"
)


read.table(
	FILENAME,
	col.names	=	CDR3_COLNAMES,
	colClasses	=	CDR3_CLASSES,
	comment.ch	=	'',
	quote		=	''
)
}
