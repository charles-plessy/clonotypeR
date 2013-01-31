## First, load accessory data if necessary.

if ( ! ( exists("V_after_C") && class(V_after_C) == "data.frame" ) )
	if ( file.exists("data/V_after_C.txt.gz") )
		V_after_C <- read.table("data/V_after_C.txt.gz", head=TRUE, row.names=1, stringsAsFactors=FALSE)
if ( ! ( exists("V_after_C") && class(V_after_C) == "data.frame" ) )
	data(V_after_C)

if ( ! ( exists("J_before_FGxG") && class(J_before_FGxG) == "data.frame" ) )
	if ( file.exists("data/J_before_FGxG.txt.gz") )
		J_before_FGxG <- read.table("data/J_before_FGxG.txt.gz", head=TRUE, row.names=1, stringsAsFactors=FALSE)
if ( ! ( exists("J_before_FGxG") && class(J_before_FGxG) == "data.frame" ) )
	data(J_before_FGxG)

if ( ! ( exists("codon_ids") && class(codon_ids) == "data.frame" ) )
	if ( file.exists("data/codon_ids.txt.gz") )
		codon_ids <- read.table("data/codon_ids.txt.gz", head=TRUE, row.names=1)
if ( ! ( exists("codon_ids") && class(codon_ids) == "data.frame" ) )
	data(codon_ids)

## Then, declare the function.

yassai.identifier <- function (data) {

if ( ! all( c("V", "J", "dna","pep") %in% names(data) ) )
  stop ("Missing V or J segment(s), or DNA or peptides sequence(s) in the data.")

if ( class(data) == 'character' )
  data <- data.frame(t(data), stringsAsFactors=F)

if ( ! class(data) == "data.frame" )
	stop ("Input must be a data frame.")

# Following function is from ?strsplit help page:
strReverse <- function(x)
	sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

V_name <- as.character(data$V)
V      <- V_after_C[V_name,]

dna    <- as.character(data$dna)

J_name <- as.character(data$J)
J      <- strReverse(J_before_FGxG[J_name,])
dnarev <- strReverse(dna)

pep    <- as.character(data$pep)

# True if the reference and CDR3 codons are identical.
is.germline <- function (ref,dna,pos) {
	answer <- toupper(substr(ref, 1, pos)) == toupper(substr(dna, 1, pos))
	answer[is.na(answer)] <- FALSE  # Replace NA per FALSE; is.germline is used in a while loop.
	return(answer)
}

germline <- function (ref, dna) {
	pos <- 1
	while( is.germline(ref,dna,pos) )
		pos <- pos + 1
	pos
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

codon2id <- function (codons)
	sapply(codons, function(X) codon_ids[X,"id"])

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
CDR3aa <- substring(CDR3aa, 1, J_germline)
CDR3aa <- substring(CDR3aa, V_germline, nchar(CDR3aa))

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
IDs <- sapply(codon2id(tocodons(substr(dna,(V_germline * 3) + 1 , (J_germline -1) * 3 ))), paste, collapse='')

# Construct and return the CDR3 in Yassai et al's nomenclature.
return( paste(CDR3aa, ".", IDs, V_name, J_name, "L", nchar(pep), sep=""))
}
