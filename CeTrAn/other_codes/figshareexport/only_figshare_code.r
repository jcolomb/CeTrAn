require(rfigshare)

#need group, metadata, Temporaryfolder, authors
authors= c("97229")

#cetran account
#login: thrawny@sourceforge.net
#pass: buridan

options(FigshareKey = "CT6SBJy8WkkGiQjhFdBBxw")
options(FigsharePrivateKey = "Ytfb4rBWwFv44eXzrzImWQ")
options(FigshareToken = "KbrzaL7fgOWEdB4RmQ4LQA084gYzMmCpNdKJGxsSTH6QKbrzaL7fgOWEdX4RmQ4LQA")
options(FigsharePrivateToken = "Uv7jjeyPByTRKXg5BM6rnw")

id_data = 98195
id_metadata_trajectory =153932
id_metadata_experiments =153933


#### here is the code for figshare. need to be done: make new articles (if necessary), get the metadata file to write on them and upload the new version, seems the ids are wrong...


 for(i in c(1:nrow(group))){
  Namecsv=paste(metadata[metadataname == "ExperimentID"],"-",99+i,".csv", sep="")
  fs_upload(id_data,Namecsv)
  }
fs_upload(id_metadata_trajectory,"metadata_trajectory.csv")
fs_upload(id_metadata_experiments,"metadata_EXP_trajectory.csv")    
# code to be written:
#### add author to the article
fs_add_authors(id_data, authors)
fs_add_authors(id_metadata_trajectory, authors)
fs_add_authors(id_metadata_experiments, authors)

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