require(devtools)
require(roxygen2)

if(Sys.info()['sysname'] == 'Windows') {
	setwd("C:/Dropbox/Projects") #Windows	
} else if(Sys.info()['sysname'] == 'Darwin') {
	setwd("~/Dropbox/Projects") #Mac
}

#Package building
document("retention")
check_doc("retention")
build("retention", binary=FALSE)
build("retention", binary=TRUE)
install("retention", build_vignettes=FALSE)
library(retention)
ls('package:retention')
check("retention")

demo(package='retention')
vignette('RetentionNEAIR')
data(students)
data(graduates)

