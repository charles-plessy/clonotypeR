unique_clonotypes <- function (libs, data) {

if ( ! is.character(libs) ) stop (
	"First argument must be a character vector of library names."
)

if ( ! is.data.frame(data) ) stop (
	"Second argument must be a data frame of clonotypes."
)

if ( FALSE %in% ( libs %in% colnames(data) ) ) stop (
	paste("Unknown library: ", libs[ ! libs %in% colnames(data) ],".", sep='', collapse=' ')
)

clonotypenames <- rownames(data)

if ( length(libs) == 1) (
	selector <- data[,libs] > 0
) else (
	selector <- rowSums( data[, libs] ) > 0
)

return(
	clonotypenames[selector]
)
}
