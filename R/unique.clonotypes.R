unique.clonotypes <- function (libs, using=clonotypes) {

if ( ! is.character(libs) ) stop (
	"Error: expects first argument to be a character vector of library names"
)

if ( ! is.data.frame(using) ) stop (
	"Error: expects second argument to be data frame of clonotypes"
)

clonotypenames <- rownames(using)

if ( length(libs) == 1) (
	selector <- clonotypes[,libs] > 0
) else (
	selector <- rowSums( clonotypes[, libs] ) > 0
)

return(
	clonotypenames[selector]
)
}
