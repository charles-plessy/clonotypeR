#' private_clonotypes
#' 
#' List clonotypes found exclusively in one library.
#' 
#' @param ... Library names.
#' @param data A clonotype table.
#' 
#' @return A vector of clonotype names.
#' 
#' @seealso \code{\link{clonotype_table}}
#' 
#' @examples
#' clonotypes <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
#' clonotypes <- clonotype_table(levels(clonotypes$lib), data=clonotypes)
#' private_clonotypes("C", data=clonotypes)

private_clonotypes <- function (..., data) {

libs <- c(...)
otherLibs <- setdiff(colnames(data), libs)

if ( ! is.character(libs) ) stop (
	"First argument must be a character vector of library names."
)

if ( ! is.data.frame(data) ) stop (
	"Data argument must be a data frame of clonotypes."
)

if ( FALSE %in% ( libs %in% colnames(data) ) ) stop (
	paste("Unknown library: ", libs[ ! libs %in% colnames(data) ],".", sep='', collapse=' ')
)

clonotypenames <- rownames(data)

# drop=FALSE otherwise rowSums fails when there is only one column.

expressedInLibs   <- rowSums(data[, libs     , drop=FALSE]) >  0
absentInOtherLibs <- rowSums(data[, otherLibs, drop=FALSE]) == 0

return(
	clonotypenames[ expressedInLibs & absentInOtherLibs ]
)
}
