clonotypes <- read.table (
  'clonotypes.txt.gz',
  head=TRUE,
  colClasses=c('factor','factor','factor','numeric','numeric','character','character','character','character')
)
