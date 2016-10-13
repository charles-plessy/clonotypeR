#' common_clonotypes
#' 
#' Reports clonotypes common between libraries.
#' 
#' When given one group of libraries, lists the clonotypes that have been
#' observed at least in one library of that group.  The returned list can be
#' used to subset a data frame produced by \code{\link{clonotype_table}}.
#' 
#' When given two groups of libraries, lists the clonotypes that have been
#' observed at least in one library of each group.  Groups can contain a single
#' library, in  which case the returned list is simply the clonotypes found in
#' both libraries.
#' 
#' When given a table of clonotypes, produces a matrix in which each cell
#' reportsquantitatively the overlap between each pair of libraries.
#' 
#' @param group1 A character vector containing clonotype library names.
#' @param group2 A character vector containing clonotype library names.
#' @param mode Only when producing a matrix of pairwise comparisons:
#'        \dQuote{count} (default) or \dQuote{abundance}, see below.
#' @param data A clonotype table where the data is stored.
#' 
#' @return 
#' 
#' In \dQuote{count} mode, each value in a matrix is the number of clonotypes
#' seen in both of the two libraries considered.  The matrix is therefore
#' symmetric.
#' 
#' In \dQuote{abundance} mode, each value indicates, for a given pair of
#' libraries, the cumulative abundance of the common clonotypes (seen in both
#' libraries), calculated for the library indicated by the row.  The matrix is
#' therefore not symmetric.  For instance, a pair of libraries A and B can have
#' 100 sequences each in total, one clonotype in common, which is found 8 times
#' in A, but 54 times in B.
#' 
#' @seealso
#' \code{\link{clonotype_table}}, \code{\link{unique_clonotypes}}
#' 
#' @examples
#' # Load example data
#' clonotypes.long <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
#' clonotypes <- clonotype_table(levels(clonotypes.long$lib), data=clonotypes.long)
#' summary(clonotypes)
#' 
#' # List clonotypes found in library A, and B or C.
#' common_clonotypes(group1="A", group2=c("B","C"), data=clonotypes)
#' 
#' # Count clonotypes found in library A, and B or C.
#' length(common_clonotypes(group1="A", group2=c("B","C"), data=clonotypes))
#' 
#' # Matrix of numbers of common clonotypes
#' common_clonotypes(data=clonotypes)
#' 
#' # Matrix of abundance of common clonotypes
#' common_clonotypes(data=clonotypes, mode="abundance")
#' 
#' @export

setGeneric(
    "common_clonotypes",
    function (group1, group2, mode, data)
        standardGeneric("common_clonotypes")
)

## When the input is a single list of libraries, return the list of common clonotypes.

#' @describeIn common_clonotypes Reports clonotypes common between libraries.

setMethod(
    common_clonotypes,
    c(group1="character", group2="missing", mode="missing", data="data.frame"),
    function(group1, data) {
       
    if ( FALSE %in% ( group1 %in% colnames(data) ) )
        stop ("Unknown library.")

    if ( length(group1) == 1) (
        selector <- data[,group1] > 0
    ) else (
        selector <- rowSums( data[, group1] ) > 0
    )       
    return (rownames(data)[selector])
})

## To Do: common_clonotypes(group1="foo", group2="bar") should return the same as common_clonotypes(group1=c("foo", "bar"))

## When the input is two lists of libraries, return the list of common
## clonotypes between the two groups, after pooling the libraries in each group.

#' @describeIn common_clonotypes Reports clonotypes common between libraries.

setMethod(
    common_clonotypes,
    c(group1="character", group2="character", mode="missing", data="data.frame"),
    function(group1, group2, data) {

    if ( FALSE %in% ( c(group1, group2) %in% colnames(data) ) )
        stop ("Unknown library.")

    intersect(
        common_clonotypes(group1=group1, data=data),
        common_clonotypes(group1=group2, data=data)
    )
})    

## When the input is a data frame, return a matrix of counts.

#' @describeIn common_clonotypes Reports clonotypes common between libraries.

setMethod(
    common_clonotypes,
    c(group1="missing", group2="missing", mode="ANY", data="data.frame"),
    function(mode="count", data) {

    if(missing(mode))
        mode <- "count"

    if ( ! (mode == "count" | mode == "abundance") )
        stop (paste (sep='', dQuote("mode"), " argument must be ", dQuote("count"), " or ", dQuote("abundance"), "."))

    numberOfLibs = dim(data)[2]

    # Prepare the matrix to hold the data
    m <- matrix(nrow=numberOfLibs, ncol=numberOfLibs)
	colnames(m) <- colnames(data)
	rownames(m) <- colnames(data)

    # Fill the matrix
    for (i in 1:numberOfLibs) {
        for (j in i:numberOfLibs) {
            commonIJ <- common_clonotypes(
                            group1=colnames(data)[i],
                            group2=colnames(data)[j],
                            data=data)
            if (mode == "count") {
                m[i,j] <- length(commonIJ)
                m[j,i] <- m[i,j]
            }
            if (mode == "abundance") {
                m[i,j] <- sum(data[commonIJ,i])
                m[j,i] <- sum(data[commonIJ,j])
            }
        }
    }

    return (m)
})

## When the input is a matrix, convert it to a data frame.

#' @describeIn common_clonotypes Reports clonotypes common between libraries.

setMethod(
    common_clonotypes,
    c(group1="missing", group2="missing", mode="ANY", data="matrix"),
    function(mode, data) {

    if(missing(mode))
        mode <- "count"

    common_clonotypes(mode=mode, data=as.data.frame(data))
})
