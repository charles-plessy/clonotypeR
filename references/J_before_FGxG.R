library(Biostrings)

file_to_load <- "J_before_FGxG.fa"

x <- read.DNAStringSet(file_to_load)

x <- as.character(x)

x <- data.frame(sequence=x, stringsAsFactors=FALSE)

write.table(x, file="../data/J_before_FGxG.txt.gz")
