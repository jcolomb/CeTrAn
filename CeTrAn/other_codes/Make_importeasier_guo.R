### import different files
###metadata (tentative)
animal = "Drosophila adults"
arena_type = "round"
arena_size = 86
light_source = 290
stimuli = "light contrast"
stimuli_type = "buridan"
frequence_of_paint = 12
recording_length = 180
resolution ="unknown"

####
EXPERIMENTID ="liu03"
timecol = 1
timecoef = 1000
## !!!!!!!! data inversed , 0:0 not set
xcol = 3
xcoef = 0.92
ycol = 2
ycoef = 0.92

center= c(100.24, 100.417) #inversed also

datafolder ="C:/Users/colomb/Desktop/cetran_data/Edgeorient"
datagroup = "C:/Users/colomb/Desktop/cetran_data/Edgeorient/group197bluegreen.txt"

#####end of inputs to change
#########################

Groups <- read.delim(datagroup, header=F)

setwd(datafolder)
folderpath= paste(datafolder, EXPERIMENTID, sep="")
dir.create(folderpath)

folder= paste(folderpath,"/","data", sep="")
  dir.create(folder)



output_csv=data.frame()


for (i in c(1: nrow(Groups))) {
  testdata = Groups[i,1]
  
  data = read.delim(paste(datafolder,testdata, sep="/"), header = F,  sep = ",", quote="\"", dec=".", fill = TRUE, skip=2)
  
  datanew =cbind (data[,timecol]*timecoef,(data[,xcol]-center[1])*xcoef, (data[,ycol]-center[2])*ycoef, data[,ycol]*0)
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


###code for determining unit
d=data.frame(datanew)
plot(d$x~d$y, type="l")
R= sqrt(d$x*d$x+d$y*d$y)
hist(d$y, xlim =c(85,95), breaks=400)
hist(-d$y, xlim =c(85,95), breaks=400)
hist(d$x, xlim =c(85,95), breaks=400)
hist(-d$x, xlim =c(85,95), breaks=400)


