yassai.nomenclature <- function (clonotype) {

# ff <- readFASTA("references/J_before_FGxG.fa", strip.descs=TRUE)

# V_after_C <- data.frame(row.names=sapply(ff, function(X) X$desc), seq=sapply(ff, function(X) X$seq), stringsAsFactors=FALSE)

# J_before_FGxG <- data.frame(row.names=sapply(ff, function(X) X$desc), seq=sapply(ff, function(X) X$seq), stringsAsFactors=FALSE)

# Following function is from ?strsplit help page:
strReverse <- function(x)
	sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

tocodons <- function (dna)
	unlist(strsplit(dna, "(?<=...)", perl=TRUE))

V <- tocodons(V_after_C[as.character(clonotype[[2]]),]) # do sanity checks !

dna <- tocodons(clonotype[[5]])

J <- tocodons(strReverse(J_before_FGxG[as.character(clonotype[[3]]),])) # do sanity checks !
dnarev <- tocodons(strReverse(clonotype[[5]]))

pep <- clonotype[[7]]

is.somatic <- function (V, dna) {
	V <- substr(V,1,3)
	dna <- substr(dna,1,3)
	tolower(V) == tolower(dna)
}

V_somatic <- 0
while( is.somatic(V[V_somatic + 1],dna[V_somatic + 1]) )
	V_somatic <- V_somatic + 1

J_somatic <- 0
while( is.somatic(J[J_somatic + 1],dnarev[J_somatic + 1]) )
        J_somatic <- J_somatic + 1

pep <- toupper(pep) # Just in case
substr(pep,1,V_somatic) <- tolower(substr(pep,1,V_somatic))
pep <- substring(pep,V_somatic)

substring(pep, nchar(pep) - J_somatic, nchar(pep)) <- tolower(substring(pep, nchar(pep) - J_somatic, nchar(pep)))
pep <- substring(pep, 1, nchar(pep) - J_somatic)

return( pep )
}
