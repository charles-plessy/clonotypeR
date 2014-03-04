is_unproductive <- function (data) {

if ( ! all( c("dna","pep") %in% names(data) ) )
  stop ("Missing DNA or peptides sequence(s) in the data.")

if ( class(data) == 'character' )
  data <- data.frame(t(data), stringsAsFactors=F)

unproductive_length     <- nchar(data$dna) %% 3 > 0
unproductive_stop       <- grepl ('\\*', data$pep)

return ( unproductive_length | unproductive_stop )
}
