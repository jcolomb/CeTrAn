# takes g_inputdir,g_filetable and g_outputdir
##
# return sample size plot


message("starting samplesize.r")
count= id_table[,c(1,2)]
for (i in c(1:nrow(id_table))) {
	count [i,3] = 1
}




n <- tapply(count[,3],count$group, sum, na.rm=T)
n <- as.matrix (n)
n <- t(n)


barplot(n, main = "samplesize")


