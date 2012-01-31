yassai.nomenclature <- function (clonotype) {

# Following function is from ?strsplit help page:
strReverse <- function(x)
	sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

# Splits a string in blocks of three.
tocodons <- function (dna)
	unlist(strsplit(dna, "(?<=...)", perl=TRUE))

# Split the reference V segment and the CDR3 nucleotidic sequence in codons.
V <- tocodons(V_after_C[as.character(clonotype[[2]]),]) # do sanity checks !
dna <- tocodons(clonotype[[5]])

# Split the reversed reference J segment and CDR3 in codons.
J <- tocodons(strReverse(J_before_FGxG[as.character(clonotype[[3]]),])) # do sanity checks !
dnarev <- tocodons(strReverse(clonotype[[5]]))

pep <- clonotype[[7]]

# True if the reference and CDR3 codons are identical.
is.somatic <- function (V, dna) {
	V <- substr(V,1,3)
	dna <- substr(dna,1,3)
	answer <- tolower(V) == tolower(dna)
	answer[is.na(answer)] <- FALSE  # Replace NA per FALSE; is.somatic is used in a while loop.
	return(answer)
}

# Counts how many consecutive codons are somatically encoded from ends to center.
V_somatic <- 0
while( is.somatic(V[V_somatic + 1],dna[V_somatic + 1]) )
	V_somatic <- V_somatic + 1

J_somatic <- 0
while( is.somatic(J[J_somatic + 1],dnarev[J_somatic + 1]) )
        J_somatic <- J_somatic + 1

# Convert to lower case and remove all but the most central somatically encoded codons.
pep <- toupper(pep) # Just in case
substr(pep,1,V_somatic) <- tolower(substr(pep,1,V_somatic))
pep <- substring(pep,V_somatic)

substring(pep, nchar(pep) - J_somatic, nchar(pep)) <- tolower(substring(pep, nchar(pep) - J_somatic, nchar(pep)))
pep <- substring(pep, 1, nchar(pep) - J_somatic)

# Convert the TRAV and TRAJ names


# Determine the ID for the remaining codons.

# Construct and return the CDR3 in Yassai et al's nomenclature.
return( pep )
}
