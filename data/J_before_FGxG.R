J_before_FGxG <- function (...) {
        # Load data/J_before_FGxG, depend on Biostrings
	if ( file.exists (system.file("data/J_before_FGxG.fa", package = "clonotypeR") ) ) {
	  file_to_load <- system.file("data/J_before_FGxG.fa", package = "clonotypeR" ) }
	if ( file.exists("data/J_before_FGxG.fa") ) {
	  file_to_load <- "data/J_before_FGxG.fa" }
	if ( file.exists("J_before_FGxG.fa") ) {
	  file_to_load <- "J_before_FGxG.fa" }
	if ( ! exists("file_to_load")) {
	  stop('Could not find the data file "J_before_FGxG.fa"') }
	data.frame(sequence=as.character(Biostrings:::read.DNAStringSet(file_to_load)), stringsAsFactors=FALSE)
}
J_before_FGxG <- J_before_FGxG()
