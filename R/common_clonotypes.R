setGeneric(
    "common_clonotypes",
    function (group1, group2, mode, data)
        standardGeneric("common_clonotypes")
)

## When the input is a single list of libraries, return the list of common clonotypes.

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

setMethod(
    common_clonotypes,
    c(group1="character", group2="character", mode="missing", data="data.frame"),
    function(group1, group2, data) {

    if ( FALSE %in% ( c(group1, group2) %in% colnames(data) ) )
        stop ("Unknown library.")

    intersect(
        common_clonotypes(group1=group1, data=clonotypes),
        common_clonotypes(group1=group2, data=clonotypes)
    )
})    

## When the input is a data frame, return a matrix of counts.

setMethod(
    common_clonotypes,
    c(group1="missing", group2="missing", mode="missing", data="data.frame"),
    function(data) {

    numberOfLibs = dim(data)[2]

    # Prepare the matrix to hold the data
    m <- matrix(nrow=numberOfLibs, ncol=numberOfLibs)
	colnames(m) <- colnames(data)
	rownames(m) <- colnames(data)

    # Fill the matrix
    for (i in 1:numberOfLibs) {
        for (j in 1:numberOfLibs) {
            m[i,j] <- length(common_clonotypes(
                                 group1=colnames(data)[i],
                                 group2=colnames(data)[j],
                                 data=data)
                            )
        }
    }

    return (m)
})

## When the input is a matrix, convert it to a data frame.

setMethod(
    common_clonotypes,
    c(group1="missing", group2="missing", mode="missing", data="matrix"),
    function(data)

    common_clonotypes(data=as.data.frame(data))
)
