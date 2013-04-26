library(Biostrings)

file_to_load <- "V_after_C.fa"

x <- read.DNAStringSet(file_to_load)

x <- as.character(x)

x <- data.frame(sequence=x, stringsAsFactors=FALSE)

write.table(x, file="../inst/extdata/V_after_C.txt.gz")
