V_after_C <- function () {
	# Load references/V_after_C.fa, depend on Biostrings
#	V_after_C <- readFASTA("references/V_after_C.fa", strip.descs=TRUE)
#	V_names <- sapply(V_after_C, function(X) X$desc)
#	V_seq <- sapply(V_after_C, function(X) X$seq)
#	data.frame( row.names=V_names, seq=V_seq, stringsAsFactors=FALSE)
	data.frame(as.character(read.DNAStringSet("references/V_after_C.fa")))
}
