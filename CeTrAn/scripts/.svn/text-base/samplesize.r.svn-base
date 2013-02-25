# takes g_inputdir,g_filetable and g_outputdir
##
# return sample size plot


message("starting samplesize.r")
for (i in c(1:nrow(id_table))) {
	id_table [i,3] = 1
}




n <- tapply(id_table[,3],id_table$group, sum, na.rm=T)
n <- as.matrix (n)
n <- t(n)


barplot(n, main = "samplesize")


