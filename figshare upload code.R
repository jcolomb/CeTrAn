## code for writing files into one fileset of figshare
#install.packages(c("rfigshare","rjson"))
require(rfigshare) 
require(rjson)
#give figshare access codes
options(FigshareKey = "jBkrLNsYbeov2oM09cXBBw")
options(FigsharePrivateKey = "LhQkDeJaVJzAhWRIKYeobA")
options(FigshareToken = "BYuYn4OjWjd8njBKCyeFXQOEAYY1MfLJ1Y0z80rVWj6AXYuYn4OjWjd8njXKCyeFXQ")
options(FigsharePrivateToken = "aC2q4lBod3Xl52CeKwp7Fg")
filesetid= "807697"
fs_auth()


setwd("/Users/choupi/Desktop/boulot/FSdata")
metafile= read.csv ("metafile.csv", header=TRUE)[,-1] 
setwd("/Users/choupi/Desktop/boulot/FSdata/alldata")

DFN= metafile$rawfilename



det=fs_download(article_id = filesetid)
##
alreadyuploaded=c()
newuploaded = c()
nup=0
newup=0
for (i in c(1: length(DFN))){
	test= DFN[i]
	if (!all(!grepl(test, det))){
			nup=nup+1
			alreadyuploaded= c(alreadyuploaded,test)
		}else{
			#toupload= test
			
			print(i)
			if (as.character(test) != "not_accessible"){
				fs_upload(article_id=filesetid, file= test)
				#scan(as.character(test),nlines = 1, what = character())
			}
			
			newup= newup +1
			newuploaded== c(newuploaded,test)
	}
}

message(paste(nup," files were already uploaded. type alreadyuploaded to get a list, type newuploaded for a list of files newly uploaded"))

