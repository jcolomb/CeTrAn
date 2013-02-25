require(rfigshare)

load.buridan.data.export <- function(params,id_prefix=NA,center=FALSE) {
  
  if (!inherits(params,"bur.xml"))
    stop("params should be of class \"bur.xml\". 
         created by the function load.buridan.xml()")
  
  # read the data files
  data <- read.table(paste(params$DATAPATH,params$DATAFILE,sep=""), header=TRUE)
  
  # TODO: check if the params are the same
 
    data$x = data$x-as.numeric(params$ARENA_CENTER_X)
    data$y = data$y-as.numeric(params$ARENA_CENTER_Y)
  
  
  #create xy coordinates
  res <- as.numeric(params$ARENA_DIAMETER_MM)/(as.numeric(params$ARENA_RADIUS)*2)
  txyb <- data.frame(time=data$time,x=data$x*res,y=data$y*res, burst=data$burst)
  
  return(txyb)

  
}

#cetran account
options(FigshareKey = "CT6SBJy8WkkGiQjhFdBBxw")
options(FigsharePrivateKey = "Ytfb4rBWwFv44eXzrzImWQ")
options(FigshareToken = "KbrzaL7fgOWEdB4RmQ4LQA084gYzMmCpNdKJGxsSTH6QKbrzaL7fgOWEdX4RmQ4LQA")
options(FigsharePrivateToken = "Uv7jjeyPByTRKXg5BM6rnw")

id_data = 98195
id_metadata_trajectory =153932
id_metadata_experiments =153933


##########read from input
Temporaryfolder="T:"
downmetadata = c()
downmetaEXPdata = c()

author= "julien colomb" 

ExperimentID = "Colomb_01"
datagroup = "D:/dokumente/data/buridan/corrected_data/groups_wt_11d.txt"

  # animal_ID
species ="Drosophila melanogaster"
gender="female"
genotype_type = "wild type"
genotype = "Wtb"
treatment = "none"

  #arena_ID
arena_name = "buridan 5° brembs"
arena_form = "round"
arena_size1 = 117
arena_size2 = "none"

  #Environment-ID: burind
walk_env = "platform"
duration = 900
tracking_resolution = 0.35
number_object = 2
  #object1_ID
o1_form = "stripe"
o1size_X = 30
o1size_Y = 0
o1size_Z = 313
o1position_X = 0
o1position_Y = 293

  #object2_ID
o2_form = "stripe"
o2size_X = 30
o2size_Y = 0
o2size_Z = 313
o2position_X = 0
o2position_Y = -293

changingvariable = "genotype"
###input for modification of the raw data

timecol = 1
timecoef = 1000
## !!!!!!!! data inversed , 0:0 not set
xcol = 3
xcoef = 0.92
ycol = 2
ycoef = 0.92

center= c(100.24, 100.417) #inversed also
#############################




 

  
####
metadataname= c("ExperimentID",
                "species", 
                "gender",
                "genotype_type" ,
                "genotype" ,
                "treatment",
                
                "arena_name",
                "arena_form",
                "arena_size1",
                "arena_size2",
                
                "walk_env",
                "duration",
                "tracking_resolution",
                "number_object" ,
                
                "o1_form", 
                "o1size_X",
                "o1size_Y" ,
                "o1size_Z" ,
                "o1position_X",
                "o1position_Y",
                
                "o2_form",
                "o2size_X",
                "o2size_Y",
                "o2size_Z", 
                "o2position_X",
                "o2position_Y")
metadata= c(ExperimentID,
            species, 
            gender,
            genotype_type ,
            genotype ,
            treatment,
            
            arena_name,
            arena_form,
            arena_size1,
            arena_size2,
            
            walk_env,
            duration,
            tracking_resolution,
            number_object ,
            
            o1_form, 
            o1size_X,
            o1size_Y ,
            o1size_Z ,
            o1position_X,
            o1position_Y,
            
            o2_form,
            o2size_X,
            o2size_Y,
            o2size_Z, 
            o2position_X,
            o2position_Y)

# read from group file
ClassX = class(metadata[metadataname == changingvariable])

group=data.frame(
  datapath = "T:test.txt" ,
group = "CS JC",
time = "1970-01-01 00:00:00"
 )
group <- read.delim(datagroup, header=F)
                    
####


if (ncol(group)<3){
  ## this is the code for buritrack data
  group <- read.delim(datagroup, header=F, colClasses=c("character",ClassX), col.names=c("datapath","group"))
 
  
  ##
  
  metadata= c(0, 0, metadata)
  metadataname= c("datapath","time", metadataname)
  output_csv= c()
  for(i in c(1:nrow(group))) #future for loop
  {
    
  metadata[metadataname == changingvariable] = group$group[i]
  
  ### load xml file and change metadata
  params <- list(load.buridan.xml(paste("D:/dokumente/data/buridan/corrected_data/",group$datapath[i], sep="")))
  env <- lapply(params, create.env.vars.center)
  
    date_of_exp = as.character(ISOdatetime(1970,1,1,0,0,0) + as.numeric(params[[1]]$TIMESTAMP))
  
  
    metadata[metadataname == "duration"] =  as.numeric(as.character(params[[1]]$DURATION))/1000 
    metadata[metadataname == "tracking_resolution"] = 1/env[[1]]$res
    metadata[metadataname == "time"] = date_of_exp
    metadata[metadataname == "datapath"] = params[[1]]$DATAFILE
    
  ### load data (centered, in mm, no other changes)
  traj = load.buridan.data.export(params[[1]],99+i, TRUE)

   Namecsv=paste(metadata[metadataname == "ExperimentID"],"-",99+i,".csv", sep="")
  
  WD=getwd()
  setwd(Temporaryfolder)
  write.csv (traj, file = Namecsv, row.names=FALSE)
  fs_upload(id_data,Namecsv)
  output_csv= rbind(output_csv, metadata)
  setwd(WD)
  }
  
colnames(output_csv)= metadataname

setwd(Temporaryfolder)
 
write.csv(rbind (downmetadata,output_csv), file = "metadata_trajectory.csv", row.names = F)
fs_upload(id_metadata_trajectory,"metadata_trajectory.csv")  
          
MET=  data.frame(metadata[metadataname == "ExperimentID"],changingvariable, author ) 
  colnames(MET)= c("experimentID", "changing variable", "author of the experiment")
write.csv(rbind (downmetaEXPdata,MET), file = "metadata_EXP_trajectory.csv", row.names = F)
fs_upload(id_metadata_experiments,"metadata_EXP_trajectory.csv")               
              
              
}

#### to do
if (ncol(group)==3){
group <- read.delim(datagroup, header=F, colClasses=c("character",ClassX,"POSIXct"))

### figshare creation
id_data = 153938
id_metadata_trajectory =153939
id_metadata_experiments =153940
  
article_categories= "Neuroscience"
id <- fs_new_article(title = "raw trajectory data", description = "format is time (in ms), x position (in mm), y position (in mm), burst (increase while the experiment is stopped and started again with the same animal). Information about the experiment is found in the metadata.", 
                     type = "fileset", tags = c("trajectory", "open science", "semanticweb", "test"), categories=article_categories ,
                     
                     visibility= "draft", 
                     links="buridan.sourceforge.net")
id_data <- id
id <- fs_new_article(title = "trajectory metadata", description = "Data to be found in the data fileset", 
                     type = "dataset", tags = c("trajectory", "open science", "semanticweb", "test"), categories=article_categories ,
                     
                     visibility= "draft", 
                     links="buridan.sourceforge.net")

id <- fs_new_article(title = "trajectory experiment metadata", description = "Experiment got their metadata: who, when, what was tested", 
                     type = "dataset", tags = c("trajectory", "open science", "semanticweb", "test"), categories=article_categories ,
                     
                     visibility= "draft", 
                     links="buridan.sourceforge.net")