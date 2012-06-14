common_clonotypes <- function (group1, group2, data=clonotypes) {

if ( ! is.character(group1) )
	stop ( paste ( dQuote("group1"), "argument must be a character vector of library names."))

if ( ! is.character(group2) )
	stop ( paste ( dQuote("group2"), "argument must be a character vector of library names."))

if ( ! length(group1) > 0 & length(group2) > 0 )
	stop ( paste ( dQuote("group1"), "or", dQuote("group2"), "argument is missing."))

if ( ! (is.data.frame(data) | is.matrix(data)) )
	stop ( paste (dQuote("data"), "argument must be a data frame or a matrix of clonotypes."))

if ( FALSE %in% ( c(group1, group2) %in% colnames(data) ) )
	stop ("Unknown library.")

found <- function (group) {
	if ( length(group) == 1) (
		selector <- data[,group] > 0
	) else (
		selector <- rowSums( data[, group] ) > 0
	)
}

selector <- found(group2) & found(group2)

return ( rownames(data)[selector])
}
