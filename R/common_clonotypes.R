common_clonotypes <- function (group1, group2, mode="groups", data) {

# Test if arguments are correct.

if ( ! (is.data.frame(data) | is.matrix(data)) )
	stop ( paste (dQuote("data"), "argument must be a data frame or a matrix of clonotypes."))

if ( ! (mode == "groups" | mode == "matrix") )
	stop (paste (sep='', dQuote("mode"), " argument must be ", dQuote("groups"), " or ", dQuote("matrix"), "."))

if (   missing("group1") & missing("group2") )
	mode <- "matrix"

if ( ! missing("group1") && ! is.character(group1) )
	stop ( paste ( dQuote("group1"), "argument must be a character vector of library names."))

if ( ! missing("group2") && ! is.character(group2) )
	stop ( paste ( dQuote("group2"), "argument must be a character vector of library names."))

found <- function (group) {
	if ( FALSE %in% ( group %in% colnames(data) ) )
		stop ("Unknown library.")

	if ( length(group) == 1) (
		selector <- data[,group] > 0
	) else (
		selector <- rowSums( data[, group] ) > 0
	)
}

# Return row names in groups mode.

if ( mode == "groups" ) {
	if ( missing("group2") ) {
		selector <- found(group1)
	} else {
		selector <- found(group1) & found(group2)
	}
	return ( rownames(data)[selector])
}

# Return a matrix of pairwise number of common clonotypes in matrix mode.

if ( mode == "matrix") {

	m <- matrix(nrow=length(colnames(data)), ncol=length(colnames(data)))

	for (i in 1:length(colnames(data)) ) {
		for (j in 1:length(colnames(data)) ) {
			m[i,j] <- length(common_clonotypes(group1=colnames(data)[i], group2=colnames(data)[j], mode="groups", data=data))
		}
	}
	colnames(m) <- colnames(data)
	rownames(m) <- colnames(data)
	return (m)
}

}
