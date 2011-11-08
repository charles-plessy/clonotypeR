clonotype.table <- function (clonotypes, libs=levels(clonotypes$lib), ...) {

countsreshape <- function (LONGTABLE) {
        reshape(
                LONGTABLE,
                direction="wide",
                idvar="name",
                timevar="lib"   )}

feat.freq <- function (LIBNAME, FEATS="J", FILTER=cdr$improductive, CDR=cdr) {
    if ( ! is.character(LIBNAME) ) {
        stop ("Must receive a library name (character vector)")
    }
    keep <- CDR$lib == LIBNAME & ! FILTER
    if (length(FEATS) == 1) {
        feat <- CDR[keep,FEATS]
    } else {
        feat <- apply(CDR[keep,FEATS], 1, paste, collapse=" ")
    }
    feat <- table(as.character(feat))
    data.frame(
        lib   =  LIBNAME,
        name  =  rownames(feat),
        count =  as.numeric(feat)
    )
}


if ( ! is.character(libs) ) {
	libs <- levels(clonotypes$lib)
}

ff <- data.frame()
for (libname in libs) {
	ff <- rbind(
		ff,
		feat.freq(libname, ...)
	)
}

ff <- countsreshape(ff)
rownames(ff) <- ff$name
colnames(ff) <- c("name",libs)
ff <- ff[,libs]
ff[is.na(ff)] <- 0
return(ff)
}
