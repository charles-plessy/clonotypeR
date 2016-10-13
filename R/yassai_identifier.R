#' yassai_identifier
#' 
#' TCR clonotype identifier (Yassai et al.)
#' 
#' The clonotype nomenclature defined by Yassai et al. in
#' \url{http://dx.doi.org/10.1007/s00251-009-0383-x}.
#' 
#' @param data A data frame or a character vector containing a clonotype(s) with
#'        proper row or element names.
#' @param V_after_C (optional) A data frame indicating the aminoacids following
#'        the conserved cystein for each V segment.
#' @param J_before_FGxG (optional) A data frame indicating the aminoacids
#'        preceding the conserved FGxG motif for each V segment.
#' @param long (optional) Avoids identifier collisions by displaying the codons,
#'        and indicating the position of the V--J junction in ambiguous cases.
#'
#' @details 
#' By default, \code{yassai_identifier()} assume mouse sequences and will load
#' the V_after_C and J_before_FGxG tables distributed in this package.  It is
#' possible to provide alternative tables either by passing them directly as
#' argument, or by installing them as \dQuote{./inst/extdata/V_after_C.txt.gz}
#' and \dQuote{./inst/extdata/J_before_FGxG.txt.gz}.
#' 
#' Some clonotypes have a different DNA sequence but the same identifier
#' following the original nomenclature (see below for examples).  The
#' \sQuote{long} mode was created to avoid these collisions.  First, it displays
#' all codons, instead of only the non-templated ones and their immediate
#' neighbors.  Second, for the clonotypes where all codons are identical to the
#' V or J germline sequence, it indicates the position of the V--J junction in
#' place of the codon IDs.
#' 
#' @return   
#' The name (for instance sIRSSy.1456B19S1B27L11) consists of five segments:
#' \enumerate{
#'    \item CDR3 amino acid identifier (ex. sIRSSy), followed by a dot;
#'    \item CDR3 nucleotide sequence identifier (ex. 1456);
#'    \item variable (V) segment identifier (ex. BV19S1);
#'    \item joining (J) segment identifier (ex. BJ2S7);
#'    \item CDR3 length identifier (ex. L11).
#'  }
#'
#' @seealso
#' \code{\link{codon_ids}}, \code{\link{J_before_FGxG}}, \code{\link{V_after_C}}
#' 
#' @examples
#' clonotypes <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
#' head(yassai_identifier(clonotypes))
#' 
#' # The following two clonotypes have a the same identifier, and are
#' # disambiguated by using the long mode
#' 
#' yassai_identifier(c(V="TRAV14-1", J="TRAJ43", dna="GCAGCTAATAACAACAATGCCCCACGA", pep="AANNNNAPR"))
#' # [1] "aAn.1A14-1A43L9"
#' 
#' yassai_identifier(c(V="TRAV14-1", J="TRAJ43", dna="GCAGCAGCTAACAACAATGCCCCACGA", pep="AAANNNAPR"))
#' # [1] "aAn.1A14-1A43L9"
#' 
#' yassai_identifier(c(V="TRAV14-1", J="TRAJ43", dna="GCAGCTAATAACAACAATGCCCCACGA", pep="AANNNNAPR"), long=TRUE)
#' # [1] "aAnnnnapr.1A14-1A43L9"
#' 
#' yassai_identifier(c(V="TRAV14-1", J="TRAJ43", dna="GCAGCAGCTAACAACAATGCCCCACGA", pep="AAANNNAPR"), long=TRUE)
#' # [1] "aaAnnnapr.1A14-1A43L9"
#' 
#' # The following two clonotypes would have the same identifier in long mode
#' # if the position of the V-J junction would not be indicated in place of the
#' # codon IDs.
#' 
#' yassai_identifier(c(V="TRAV14N-1", J="TRAJ56", dna="GCAGCTACTGGAGGCAATAATAAGCTGACT", pep="AATGGNNKLT"), long=TRUE)
#' # [1] "aatggnnklt.1A14N1A56L10"
#' 
#' yassai_identifier(c(V="TRAV14N-1", J="TRAJ56", dna="GCAGCAACTGGAGGCAATAATAAGCTGACT", pep="AATGGNNKLT"), long=TRUE)
#' # [1] "aatggnnklt.2A14N1A56L10"
#' 
#' @importFrom utils read.table
#' 
#' @export yassai_identifier

setGeneric(
    "yassai_identifier",
    function(data, V_after_C, J_before_FGxG, long=FALSE)
    standardGeneric("yassai_identifier")
)

# Case of a single clonotype:
# convert the data vector to a one-line data frame and call the function again.

#' @describeIn yassai_identifier TCR clonotype identifier (Yassai et al.)

setMethod(
    yassai_identifier,
    c(data="character", V_after_C="data.frame", J_before_FGxG="data.frame", long="ANY"),
    function(data, V_after_C, J_before_FGxG, long) {
    if(missing(long)) long <- FALSE
    yassai_identifier(
        data.frame(t(data), stringsAsFactors=F),
        V_after_C,
        J_before_FGxG,
        long)
})

# Load default data if no V_after_C and J_before_FGxG tables are specified.
# Custom data in "./inst/extdata/" has precedence.

#' @describeIn yassai_identifier TCR clonotype identifier (Yassai et al.)

setMethod(
    yassai_identifier,
    c(data="ANY", V_after_C="missing", J_before_FGxG="missing", long="ANY"),
    function(data, long) {
    if ( file.exists("inst/extdata/V_after_C.txt.gz") ) {
        V_after_C <- read.table("inst/extdata/V_after_C.txt.gz", header=TRUE, row.names=1, stringsAsFactors=FALSE)
        warning("Loading custom data from 'inst/extdata/V_after_C.txt.gz'.")
    } else {
        V_after_C <- read.table(system.file('extdata', 'V_after_C.txt.gz', package = "clonotypeR"), stringsAsFactors=FALSE)
    }
    if ( file.exists("inst/extdata/J_before_FGxG.txt.gz") ) {
        J_before_FGxG <- read.table("inst/extdata/J_before_FGxG.txt.gz", header=TRUE, row.names=1, stringsAsFactors=FALSE)
        warning("Loading custom data from 'inst/extdata/J_before_FGxG.txt.gz'.")
    } else {
        J_before_FGxG <- read.table(system.file('extdata', 'J_before_FGxG.txt.gz', package = "clonotypeR"), stringsAsFactors=FALSE)
    }
    if ( missing(long) ) long <- FALSE
    yassai_identifier(data, V_after_C, J_before_FGxG, long)
})

# Main function.

#' @describeIn yassai_identifier TCR clonotype identifier (Yassai et al.)

setMethod(
    yassai_identifier,
    c(data="data.frame", V_after_C="data.frame", J_before_FGxG="data.frame", long="logical"),
    function(data, V_after_C, J_before_FGxG, long=FALSE) {

if ( ! ( exists("codon_ids") && class(codon_ids) == "data.frame" ) )
	if ( file.exists("inst/extdata/codon_ids.txt.gz") )
		codon_ids <- read.table("inst/extdata/codon_ids.txt.gz", header=TRUE, row.names=1)
if ( ! ( exists("codon_ids") && class(codon_ids) == "data.frame" ) )
	codon_ids <- read.table(system.file('extdata', 'codon_ids.txt.gz', package = "clonotypeR"), header=TRUE, row.names=1)

if ( ! all( c("V", "J", "dna","pep") %in% names(data) ) )
  stop ("Missing V or J segment(s), or DNA or peptides sequence(s) in the data.")

strReverse <- function(x)   # From ?strsplit help page.
	sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

V_name <- as.character(data$V)
V      <- as.character(V_after_C[V_name,])

dna    <- as.character(data$dna)

J_name <- as.character(data$J)
J      <- strReverse(as.character(J_before_FGxG[J_name,]))
dnarev <- strReverse(dna)

pep    <- as.character(data$pep)

# True if the reference and CDR3 codons are identical.
is.germline <- function (ref,dna,pos) {
	if (pos > nchar(dna)) return (FALSE)
	answer <- toupper(substr(ref, 1, pos)) == toupper(substr(dna, 1, pos))
	answer[is.na(answer)] <- FALSE  # Replace NA per FALSE; is.germline is used in a while loop.
	return(answer)
}

germline <- function (ref, dna) {
	pos <- 1
	while( is.germline(ref,dna,pos) )
		pos <- pos + 1
	return(pos)
}

# Example
# 
# Germline-encoded sequence stops at codon 2 (V_germline).
#             T   T  |
#            GCA GCA AGT G
#   TRAV14-1 GCA GCA TCT TAT AAC CAG GGG AAG CTT ATC TRAJ23 nchar = 30
#                  G AAT TAT AAC CAG GGG AAG CTT ATC
#                     |   T   T   T   T   T   T   T
# Germline-encoded sequence restarts at codon 4 (J_germline).

V_germline <- ceiling(                apply(cbind(V,dna),    1, function(X) germline(X[1], X[2]))      / 3 ) - 1
J_germline <- ceiling( ( nchar(dna) - apply(cbind(J,dnarev), 1, function(X) germline(X[1], X[2])) + 1) / 3 ) + 1

tocodons <- function (sequences)
	strsplit(sequences, "(?<=...)", perl=TRUE)

codon2id <- function (codons) {
    sapply(codons, function(X) {
        if (length(X) == 0) {
            return('')
        } else {
            return(codon_ids[X,"id"])
        }
    })
}
###################


#Remains to do: 
#
# - Convert to lowercase where applicable
# - get codon ids for remaining residues
# Return something for unproductive clonotypes

##################


# Convert to lower case and remove all but the most central germlineally encoded codons.
CDR3aa <- toupper(pep) # Just in case
substr(CDR3aa, 1, V_germline)              <- tolower(substr(CDR3aa, 1, V_germline))
substr(CDR3aa, J_germline, nchar(CDR3aa))  <- tolower(substr(CDR3aa, J_germline, nchar(CDR3aa)))

# The published version of the Yassai identifier has "collisions": clonotypes with different
# DNA sequences but same identifiers.  As a workaround, the "long" option skips the trimming
# of the leftmost and rightmost unmodified germline codons.

if (long == FALSE) {
    CDR3aa <- substring(CDR3aa, 1, J_germline)
    CDR3aa <- substring(CDR3aa, V_germline, nchar(CDR3aa))
}

# Convert the V and J names
V_name <- sub("TRAV","A",V_name)
V_name <- sub("N-",  "N",V_name)
V_name <- sub("D-",  "D",V_name)
V_name <- sub("/.*", "" ,V_name)
V_name <- sub("TRBV","B",V_name)
V_name <- sub("TRGV","G",V_name)
V_name <- sub("TRDV","D",V_name)
J_name <- sub("TRAJ","A",J_name)
J_name <- sub("TRBJ","B",J_name)
J_name <- sub("TRGJ","G",J_name)
J_name <- sub("TRDJ","D",J_name)

# Determine the ID for the remaining codons.
IDs <- codon2id(tocodons(substr(dna,(V_germline * 3) + 1 , (J_germline -1) * 3 )))

# Disambiguate blunt V/J recombinations where all codons are like either V or J germline.
# These kind of collisions only happen with the "long" format.
# Example:
# > data
#           V      J                            dna        pep
# 1 TRAV14N-1 TRAJ56 GCAGCTACTGGAGGCAATAATAAGCTGACT AATGGNNKLT
# 2 TRAV14N-1 TRAJ56 GCAGCAACTGGAGGCAATAATAAGCTGACT AATGGNNKLT
# > yassai_identifier(data, long=T)
# [1] "aatggnnklt.1A14N1A56L10" "aatggnnklt.2A14N1A56L10"
# > yassai_identifier(data, long=F)
# [1] "aa.1A14N1A56L10" "at.2A14N1A56L10"

IDs[IDs == ''] <- V_germline[IDs == '']

## Different paste commands are needed if the input is one or multiple clonotypes.

if ( nrow(data) == 1 )  {
  IDs <- paste(IDs, collapse='')
} else {
  IDs <- sapply(IDs, paste, collapse='')
}

# Construct and return the CDR3 in Yassai et al's nomenclature.
return( paste(CDR3aa, ".", IDs, V_name, J_name, "L", nchar(pep), sep=""))

})
