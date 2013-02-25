# creates a barplot with errorbars
mybarplot <- function(means,sds,rownames,...) {
	mp <- barplot2(means,plot.grid = TRUE,names.arg=rownames,
	ci.l = means-sds, ci.u = means+sds,plot.ci = TRUE,...)
	box()
}
