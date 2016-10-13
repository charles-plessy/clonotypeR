#' unique_clonotypes
#' 
#' Lists unique clonotypes in libraries
#' 
#' Finds all the clonotypes expressed in one or more libraries, and returns a
#' vector where they are listed once.  This vector can be used to subset a
#' clonotype_table.
#' 
#' @param ... One or more character vectors contain clonotype library names.
#' @param data The name of the clonotype_table where the data is stored.
#' 
#' @return 
#' Character vector of clonotype names.  Their order follows the original row
#' name order of the clonotype_table.
#' 
#' @seealso
#' \code{\link{clonotype_table}}, \code{\link{common_clonotypes}}
#' 
#' @examples
#' # Load example data
#' clonotypes.long <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
#' clonotypes <- clonotype_table(levels(clonotypes.long$lib), data=clonotypes.long)
#' summary(clonotypes)
#' 
#' # List clonotypes found in library A.
#' unique_clonotypes("A", data=clonotypes)
#' 
#' # List clonotypes found in library A or B.
#' unique_clonotypes("A","B", data=clonotypes)
#' 
#' @export

unique_clonotypes <- function (..., data) {

libs <- c(...)

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
