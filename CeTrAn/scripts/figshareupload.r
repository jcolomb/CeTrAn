#push data on Figshare


###INPUTS
Experimentname = "test"
authorname ="me"
species = "drosophila melanogaster"

###this should be there already:
fileName   #should be the list of files of raw data

# authentication to figshare data
# we will use my accounts, because I can still use the old API there.
require(rfigshare) 
## need to sign up with my crendentials:
#julien.colomb@fu-berlin.de
#forCETRAN
fs_auth()

#figshareid for matadata upload
id_meta=3409501


             
               





### load raw data
#get the path to the data:

#xml metadata files: xmlfiles
xmlfiles=paste(g_inputdir,fileName,sep="/")

#data files: read from the xml: datafiles
datapath = strsplit(xmlfiles[1],"/")	

datapath = paste(c(datapath[[1]][1:length(datapath[[1]])-1],""),collapse="/")


datafiles= c()
for (i in c(1:length(params))) {
  datafiles= c(datafiles, paste(datapath,params[[i]]$DATAFILE, sep=""))
}

#grouping file

id_table$xml=xmlfilesname
temp=strsplit(fileName,"/")	
xmlfilesname=c()
for (i in c(1:length(temp))){
  xmlfilesname= c(xmlfilesname, temp[[i]] [length(temp[[1]])])
  
}
temp = paste(c(temp[[1:length(temp)]] [length(temp[[1]])],""),collapse="")

write.csv(id_table[,3:2], file = "grouping.csv", row.names = F, col.names = F)



#push it to figshare, raw data:

# create a new article for this project:
idraw = fs_create(paste("rawdata for experiment", experimentname, sep = " "),
               description = "--",
               type = "dataset")
# put everyting up
fs_update(idraw, description= "files to perform the CETRAN analysis are here, the path to the xml files might be different in the groups txt file.")
fs_upload(idraw, datafiles)
fs_upload(idraw, xmlfiles)
fs_upload(idraw, "grouping.csv")
fs_add_categories(idraw,"Neuroscience")
fs_add_tags(idraw, "CETRAN_raw_data")
# we should change the liscence to CC0 here


down=fs_download(idraw)

## put the metadata up:

Metadata_experiment = data.frame(
  "Author_name" = authorname,
  "figshareID_rawfiles" =idraw,
  "Experiment"= Experimentname,
  "species"= species,
  "date" = format(Sys.Date(), "%Y %m %d") ,
  "grouping"= paste0(levels(g_filetable[,2]), collapse = ",")
)

metadatafilename=paste0(format(Sys.Date(), "%Y_%m_%d"),Experimentname,"meta.csv")
write.csv(Metadata_experiment, file = metadatafilename, row.names = F)
fs_upload(id_meta, metadatafilename)
