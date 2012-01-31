J_before_FGx <- function () {
	# Load references/J_before_FGxG.fa, depend on Biostrings
	J_after_C <- readFASTA("references/J_before_FGxG.fa", strip.descs=TRUE)
	J_names <- sapply(J_after_C, function(X) X$desc)
	J_seq <- sapply(J_after_C, function(X) X$seq)
	data.frame( row.names=J_names, seq=J_seq, stringsAsFactors=FALSE)
}
