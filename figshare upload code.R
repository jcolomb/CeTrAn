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

det=fs_download(article_id = filesetid)
det[1]
test="20_PKCRNAI_106.csv"

alreadyuploaded=c()
newuploaded== c()
nup=0
if (grep(test, det)>0){
	nup=nup+1
	alreadyuploaded= c(alreadyuploaded,test)
}else{
	toupload= test
	fs_upload(article_id=filesetid, file= test)
	newuploaded== c(newuploaded,test)
}
message(paste(nup," files were already uploaded. type alreadyuploaded to get a list, type newuploaded for a list of files newly uploaded"))

metafile= read.csv ("metafile.csv", header=TRUE)[,-1], 