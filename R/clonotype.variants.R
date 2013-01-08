clonotype.variants <- function (libs, feats=c("V","pep","J"), data=clonotypes) {

# TODO: check if "variant" has a precise meaning that differs.

if ( missing (libs) )
	libs <- levels(data$lib)

if ( ! is.character(libs) )
        stop ("Include list of libraries as first argument.")

if ( ! length(libs) == length(unique(libs)) )
	stop ("Redundant list of libraries.")

if ( ! is.data.frame(data) )
	stop ("Input clonotypes must be in a data frame.")

# The following function counts, for a single library, the occurrences of
# segments, CDR3s or combinations of them, and return them as a simple data
# frame of tuples describing in which library, which combination was found how
# many times.  By default it looks for the libraries in the ‘clonotypes’ table
# and discards the unproductive rearrangements.

feat.freq <- function (lib, filter) {
    keep <- from$lib == lib & (! filter) & from$score >= minscore & from$mapq >= minqual
    if (length(feats) == 1) {
        feat <- from[keep,feats]
    } else {
        feat <- apply(from[keep,feats], 1, paste, collapse=" ")
    }
    feat <- table(as.character(feat))
    data.frame(
        lib   =  lib,
        name  =  rownames(feat),
        count =  as.numeric(feat)
    )
}

# For each library, produce tables of tuples (see above), and append them to a
# data frame.

ff <- data.frame()
for (libname in libs) {
    ff <- rbind(
        ff,
        feat.freq(libname, filter)
    )
}

ff <- reshape(
    ff,
    direction="wide",
    idvar="name",
    timevar="lib"
)
rownames(ff) <- ff$name
colnames(ff) <- c("name",libs)
ff <- ff[, libs, drop=FALSE]
ff[is.na(ff)] <- 0
return(ff)
}
