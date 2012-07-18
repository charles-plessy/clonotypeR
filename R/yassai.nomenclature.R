## First, load accessory data if necessary.

if ( ! ( exists("V_after_C") && class(V_after_C) == "data.frame" ) )
	if ( file.exists("data/V_after_C.R") )
		source("data/V_after_C.R")
if ( ! ( exists("V_after_C") && class(V_after_C) == "data.frame" ) )
	data(V_after_C)

if ( ! ( exists("J_before_FGxG") && class(J_before_FGxG) == "data.frame" ) )
	if ( file.exists("data/J_before_FGxG") )
		source("data/J_before_FGxG.R")
if ( ! ( exists("J_before_FGxG") && class(J_before_FGxG) == "data.frame" ) )
	data(J_before_FGxG)

if ( ! ( exists("codon_ids") && class(J_before_FGxG) == "codon_ids" ) )
	if ( file.exists("data/codon_ids.txt.gz") )
		codon_ids <- read.table("data/codon_ids.txt.gz", head=TRUE, row.names=1)
if ( ! ( exists("codon_ids") && class(J_before_FGxG) == "codon_ids" ) )
	data(codon_ids)

## Then, declare the function.

yassai.nomenclature <- function (clonotype) {

# Following function is from ?strsplit help page:
strReverse <- function(x)
	sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

# Splits a string in blocks of three.
tocodons <- function (dna)
	unlist(strsplit(dna, "(?<=...)", perl=TRUE))

# Split the reference V segment and the CDR3 nucleotidic sequence in codons.
V_name <- as.character(clonotype[[2]])
V <- tocodons(V_after_C[V_name,]) # do sanity checks !
dna <- tocodons(clonotype[[5]])

# Split the reversed reference J segment and CDR3 in codons.
J_name <- as.character(clonotype[[3]])
J <- tocodons(strReverse(J_before_FGxG[J_name,])) # do sanity checks !
dnarev <- tocodons(strReverse(clonotype[[5]]))

pep <- clonotype[[7]]

# True if the reference and CDR3 codons are identical.
is.germline <- function (V, dna) {
	V <- substr(V,1,3)
	dna <- substr(dna,1,3)
	answer <- tolower(V) == tolower(dna)
	answer[is.na(answer)] <- FALSE  # Replace NA per FALSE; is.germline is used in a while loop.
	return(answer)
}

# Counts how many consecutive codons are germlineally encoded from ends to center.
V_germline <- 0
while( is.germline(V[V_germline + 1],dna[V_germline + 1]) )
	V_germline <- V_germline + 1

J_germline <- 0
while( is.germline(J[J_germline + 1],dnarev[J_germline + 1]) )
        J_germline <- J_germline + 1

# Convert to lower case and remove all but the most central germlineally encoded codons.
CDR3aa <- toupper(pep) # Just in case
substr(CDR3aa,1,V_germline) <- tolower(substr(CDR3aa,1,V_germline))
CDR3aa <- substring(CDR3aa,V_germline)

substring(CDR3aa, nchar(CDR3aa) - J_germline, nchar(CDR3aa)) <- tolower(substring(CDR3aa, nchar(CDR3aa) - J_germline, nchar(CDR3aa)))
CDR3aa <- substring(CDR3aa, 1, nchar(CDR3aa) - J_germline)

# Convert the TRAV and TRAJ names
# To Do !

# Determine the ID for the remaining codons.
IDs <- paste(codon_ids[dna[(V_germline +1) : (nchar(pep) - J_germline -1)],"id"], collapse="")
if ( (V_germline +1) > (nchar(pep) - J_germline -1) ) IDs <- ""

# Construct and return the CDR3 in Yassai et al's nomenclature.
return( paste(CDR3aa, ".", IDs, V_name, J_name, "L", nchar(pep), sep=""))
}
