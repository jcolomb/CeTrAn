#loads the libraries
#setwd("C:/Users/colomb/Desktop/buridan_onsvn/CeTrAn/")
# setwd("/Users/choupi/CeTrAn/CeTrAn/")
#setwd("D:/dokumente/GitHub/CeTrAn/CeTrAn/")
res <- suppressMessages(
	c(require("adehabitatLT"),#require("adehabitat"),
	require("XML"),
	require("lattice"),
	require("hexbin"),
	require("colorRamps"),
	require("rgl"),
	require("gplots"),
  require("corrplot")
   )
)
if (any(res==FALSE)) 
stop("Could not find all required packages. Make sure all following packages are installed.
run the install_package.r on our R GUI (found in the install folder)",
call. = FALSE, domain = NA)

# sets options
options("digits.secs" = 3)
	   
# loads the analysis functions
source("functions/utils.r", local=TRUE)
source("functions/newfct.r", local=TRUE)
source("functions/functions.r", local=TRUE)
source("functions/plot_functions.r", local=TRUE)
# source("functions/defaultvalues.r" local=TRUE)