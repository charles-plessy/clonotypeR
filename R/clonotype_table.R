#' clonotype_table
#' 
#' Create a table count of clonotypes or other features
#' 
#' Using a clonotype data frame loaded with \code{\link{read_clonotypes}},
#' \code{clonotype_table} will create a table counting how many times each
#' clonotypes have been seen in each libraries.  By default, the unproductive
#' rearrangements are filtered out.
#' 
#' @param libs A character vector containing the name of one or many libraries.
#'        Same names must not appear twice.  If no library names are provided,
#'        all the libraries present in the clonotypes data frame will be used.
#'        
#' @param feats What to count.  By default, it counts clonotypes, defined as
#'        \code{c("V","pep","J")}. But it can also count single features, such
#'        as the V or J segments.
#' 
#' @param data Data frame as loaded by \code{\link{read_clonotypes}}.
#' 
#' @param filter Logical vector to filter out clonotypes.  By default it relies
#'        on the clonotypes data frame to provide a \dQuote{unproductive} column
#'        that indicates clonotypes with a stop codon or a frame shift, and a
#'        \dQuote{ambiguous} column that indicates clonotypes where the DNA
#'        sequences has ambiguous (\dQuote{N}) nucleotides.
#'        
#' @param minscore Minimum alignment score.  Clonotypes with an alignment score
#'        lower than this value are discarded.
#'        
#' @param minqual Minimum mapping quality.  Clonotypes with a mapping quality
#'        lower than this value are discarded.
#'        
#' @param sample Indicate the number of clonotypes to randomly sample from the
#'        library (no replacement).  Default: no 
#' 
#' @return 
#' \code{\link{clonotype_table}} returns a data frame, where row names are
#' features (clonotypes, segment names, \dots), column names are libraries, and
#' values are number of times each feature was found in each library.
#' 
#' @seealso 
#' \code{\link{read_clonotypes}}
#' 
#' @examples
#' 
#' # Read the package's example data
#' clonotypes <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
#' 
#' # Inspect the alignment scores
#' hist(clonotypes$score)
#' 
#' # Count J segments
#' j <- clonotype_table(levels(clonotypes$lib), "J", data=clonotypes)
#' 
#' # Normalise counts in parts per million
#' J <- data.frame(prop.table(as.matrix(j),2) * 1000000)
#' 
#' @importFrom stats reshape
#'  
#' @export

clonotype_table <- function (libs, feats=c("V","pep","J"), data, filter=(data$unproductive | data$ambiguous), minscore=0, minqual=1, sample=FALSE) {

if ( missing (libs) )
	libs <- levels(data$lib)

if ( ! is.character(libs) )
        stop ("Include list of libraries as first argument.")

if ( ! length(libs) == length(unique(libs)) )
	stop ("Redundant list of libraries.")

if ( ! is.data.frame(data) )
	stop ("Input clonotypes must be in a data frame.")

if ( ! is.logical(data$unproductive) )
	stop ('Input missing "unproductive" column.')

if ( ! is.logical(data$ambiguous) )
	stop ('Input missing "ambiguous" column.')

# The following function counts, for a single library, the occurrences of
# segments, CDR3s or combinations of them, and return them as a simple data
# frame of tuples describing in which library, which combination was found how
# many times.  By default it discards the unproductive rearrangements.

feat.freq <- function (lib, filter) {
    if ( all(c('score', 'mapq') %in% colnames(data) ) ) {
      keep <- data$lib == lib & (! filter) & data$score >= minscore & data$mapq >= minqual
    } else {
      keep <- data$lib == lib & (! filter)
    }
    if (length(feats) == 1) {
        feat <- data[keep,feats]
    } else {
        feat <- apply(data[keep,feats], 1, paste, collapse=" ")
    }
    if (sample) {
        feat <- sample(feat, sample)
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
