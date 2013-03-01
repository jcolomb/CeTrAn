require(rfigshare)

#need ExperimentID, group, metadata, Temporaryfolder, authors from previous code
#authors= c("97229")
#ExperimentID ="Colomb_01"

#cetran account
#login: thrawny@sourceforge.net
#pass: buridan

options(FigshareKey = "CT6SBJy8WkkGiQjhFdBBxw")
options(FigsharePrivateKey = "Ytfb4rBWwFv44eXzrzImWQ")
options(FigshareToken = "KbrzaL7fgOWEdB4RmQ4LQA084gYzMmCpNdKJGxsSTH6QKbrzaL7fgOWEdX4RmQ4LQA")
options(FigsharePrivateToken = "Uv7jjeyPByTRKXg5BM6rnw")

id_data = 153938
id_metadata_trajectory =153939
id_metadata_experiments =153940


#### here is the code for figshare. need to be done: make new articles (if necessary), get the metadata file to write on them and upload the new version, seems the ids are wrong...


 for(i in c(1:nrow(group))){
  Namecsv=paste(metadata[metadataname == "ExperimentID"],"-",99+i,".csv", sep="")
  fs_upload(id_data,Namecsv)
  }
fs_upload(id_metadata_trajectory,"metadata_trajectory.csv")
fs_upload(id_metadata_experiments,"metadata_EXP_trajectory.csv")    

#### add author to the article
if (!is.na (authors)){
  fs_add_authors(id_data, authors)
  fs_add_authors(id_metadata_trajectory, authors)
  fs_add_authors(id_metadata_experiments, authors)
}


# code to be written:

# need to read the experiment metadata, check that the new experimentID was never entered before
#if the ID was entered, ask if new experiment or new data for the experiment
#1new experiment -> ask for a new ID
#2new data -> ?? (I have to think about it: add only the new data and metadata, ...)
# If new ID: change the metadata files and add raw data files

IDME = fs_details(id_metadata_experiments)
file = IDME$files[[1]]$download_url
IDME_dat= read.csv (file, header= T)
ExpID_list=IDME_dat[,1]
ExpID_list
class(ExpID_list)
length(ExpID_list)
T=0
for (i in c(1,length(ExpID_list))){
  if (ExperimentID == ExpID_list[i]) T=T+1
}
##old code
### figshare article creation/modification
# 
#   fs_browse(id_data)
# article_categories= "Neuroscience"
# 
# id <- fs_new_article(title = "raw trajectory data", description = "format is time (in ms), x position (in mm), y position (in mm), burst (increase while the experiment is stopped and started again with the same animal). Information about the experiment is found in the metadata.", 
#                      type = "fileset", tags = c("trajectory", "open science", "semanticweb", "test"), categories=article_categories ,
#                      
#                      visibility= "draft", 
#                      links="buridan.sourceforge.net")
# id_data <- id
# id <- fs_new_article(title = "trajectory metadata", description = "Data to be found in the data fileset", 
#                      type = "dataset", tags = c("trajectory", "open science", "semanticweb", "test"), categories=article_categories ,
#                      
#                      visibility= "draft", 
#                      links="buridan.sourceforge.net")
# 
# id <- fs_new_article(title = "trajectory experiment metadata", description = "Experiment got their metadata: who, when, what was tested", 
#                      type = "dataset", tags = c("trajectory", "open science", "semanticweb", "test"), categories=article_categories ,
#                      
#                      visibility= "draft", 
#                      links="buridan.sourceforge.net")