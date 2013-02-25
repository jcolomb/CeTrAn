### import different files
###metadata (tentative)
animal = "Drosophila adults"
arena_type = "ball"
arena_size = 10000
light_source = 150
stimuli = "image"
stimuli_type = "none"
frequence_of_paint = 20
recording_length = 120000
resolution ="unknown"

####
EXPERIMENTID ="AB01"
timecol = 1
timecoef = 1000

xcol = 5
xcoef = 10
ycol = 6
ycoef = 10

datafolder ="C:/Users/colomb/Desktop/arminsdata/"
datagroup = "C:/Users/colomb/Desktop/arminsdata/Groups.txt"

#####end of inputs to change
#########################

Groups <- read.delim(datagroup, header=F)

setwd(datafolder)
folderpath= paste(datafolder, EXPERIMENTID, sep="")
dir.create(folderpath)

folder= paste(folderpath,"/","data", sep="")
  dir.create(folder)



output_csv=data.frame()


for (i in c(1: length(filenames))) {
  testdata = Groups[i,1]
  
  data = read.csv(paste(datafolder,testdata, sep=""), header = F,  sep = "\t", quote="\"", dec=".", fill = TRUE)
  
  datanew =cbind (data[,timecol]*timecoef,data[,xcol]*xcoef, data[,ycol]*ycoef, data[,ycol]*0)
  colnames (datanew)=c("date","x","y", "burst")
  
  
  shortpath= paste("data","/",Groups[i,2],"_",i,".csv",sep="")
  dataexportpath=paste(datafolder,EXPERIMENTID,"/", shortpath,sep="")
  
  
  write.csv (datanew, file=dataexportpath,row.names=FALSE)
  
  
  output_i= data.frame(Groups[i,3],paste(Groups[i,2],"_",i),paste(Groups[i,2]), shortpath, animal, arena_type,arena_size,light_source, stimuli, stimuli_type,frequence_of_paint, recording_length, resolution)
  #
  
  output_csv= rbind(output_csv, output_i)
  
}
names(output_csv)=c("date","id","group", "path", "animal","arena_type", "arena_size","light_source","stimuli", "stimuli_type", "frequence_of_paint","duration","pixel_size_mm" )

setwd(folderpath)
write.csv(output_csv, file="metadata.csv",row.names=FALSE)

