unique_clonotypes <- function (libs, from=clonotypes) {

if ( ! is.character(libs) ) stop (
	"First argument must be a character vector of library names."
)

if ( ! is.data.frame(from) ) stop (
	"Second argument must be a data frame of clonotypes."
)

if ( FALSE %in% ( libs %in% colnames(from) ) ) stop (
	paste("Unknown library: ", libs[ ! libs %in% colnames(from) ],".", sep='', collapse=' ')
)

clonotypenames <- rownames(from)

if ( length(libs) == 1) (
	selector <- from[,libs] > 0
) else (
	selector <- rowSums( from[, libs] ) > 0
)

return(
	clonotypenames[selector]
)
}
