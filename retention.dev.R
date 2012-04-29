install.packages(c('devtools', 'roxygen2', 'RSQLite', 'ipeds'), 
		repos=c('http://cran.r-project.org', 'http://r-forge.r-project.org'))

require(devtools)
require(roxygen2)
require(RSQLite)
require(ipeds)

setwd("~/Dropbox/Projects") #Mac
setwd("C:/Dropbox/Projects") #Windows

#Package building
document("retention")
check_doc("retention")
build("retention", binary=FALSE)
build("retention", binary=TRUE)
install("retention")
check("retention")
library(retention)
ls('package:retention')

