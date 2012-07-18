V_after_C <- function (...) {
        # Load references/V_after_C.fa, depend on Biostrings
	if ( file.exists (system.file("data/V_after_C.fa", package = "clonotypeR") ) ) {
	  file_to_load <- system.file("data/V_after_C.fa", package = "clonotypeR" ) }
	if ( file.exists("data/V_after_C.fa") ) {
	  file_to_load <- "data/V_after_C.fa" }
	if ( file.exists("V_after_C.fa") ) {
	  file_to_load <- "V_after_C.fa" }
	if ( ! exists("file_to_load")) {
	  stop('Could not find the data file "V_after_C.fa"') }
	data.frame(sequence=as.character(Biostrings:::read.DNAStringSet(file_to_load)), stringsAsFactors=FALSE)
}
V_after_C <- V_after_C()

