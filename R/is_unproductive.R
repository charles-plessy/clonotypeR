#' is_unproductive
#' 
#' Determines if clonotype sequences are productive.
#' 
#' ClonotypeR identifies V and J segments, isolates the DNA sequence between the
#' conserved cystein and the FGxG motifs, and translates it.  This functions
#' verifies that this sequence is in frame and has no stop codon.
#' 
#' @param data Data frame of clonotype sequences, or character vector
#'        describing a single clonotype, where the DNA sequence is available
#'        under the name \dQuote{dna} and its translation available under the
#'        name \dQuote{pep}.
#'        
#' Clonotypes are marked unproductive if the length of their DNA sequence is not
#' a multiple of 3, or if they contain a stop codon, as indicated by an
#' asterisk in the translated sequence.
#' 
#' @return
#' 
#' Logical vector, with one value per row in the original data.
#' 
#' @seealso
#' 
#' \code{\link{read_clonotypes}}
#' 
#' @examples 
#' clonotypes <- read_clonotypes(system.file('extdata', 'clonotypes.txt.gz', package = "clonotypeR"))
#' is_unproductive(clonotypes)
#' 
#' @export

is_unproductive <- function (data) {

if ( ! all( c("dna","pep") %in% names(data) ) )
  stop ("Missing DNA or peptides sequence(s) in the data.")

if ( class(data) == 'character' )
  data <- data.frame(t(data), stringsAsFactors=F)

unproductive_length     <- nchar(data$dna) %% 3 > 0
unproductive_stop       <- grepl ('\\*', data$pep)

return ( unproductive_length | unproductive_stop )
}
