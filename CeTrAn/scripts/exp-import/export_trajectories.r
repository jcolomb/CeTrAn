


if (!g_manual) {
arena_size = env[[1]]$r
light_source =env[[1]]$outer_r
stimuli_type = env[[1]]$stripe_w
recording_length = env[[1]]$duration
}

setwd(outputpath)
folderpath= paste(outputpath,"/", EXPERIMENTID, sep="")
dir.create(folderpath)


for(j in c(1: length(levels(id_table$group)))){
  folder= paste(folderpath,"/",levels(id_table$group)[j], sep="")
  dir.create(folder)
}
output_csv=data.frame()
#id_table$id[i],id_table$group[i], dataexportpath, animal, arena_type,env[[1]]$r,env[[1]]$outer_r, stimuli, env[[1]]$stripe_w,frequence_of_paint,)

                      

for (i in c(1: nrow(id_table))){
  if (!g_manual) {
arena_size = env[[i]]$r
light_source =env[[i]]$outer_r
stimuli_type = env[[i]]$stripe_w
recording_length = env[[i]]$duration/1000
}
date_of_exp = as.character(ISOdatetime(1970,1,1,0,0,0) + as.numeric(params[[i]]$TIMESTAMP))
resolution =  1/env[[i]]$res 
  data= ltraj2traj(traj[id(traj)==id_table$id[i]])
  datas =data.frame(data[,c(5,3:4,2)])
  datas[,1]= datas[,1]-datas[1,1]
  
  shortpath= paste(EXPERIMENTID,"/",id_table$group[i],"/",id_table$id[i],".csv",sep="")
  dataexportpath= paste(folderpath,"/",id_table$group[i],"/",id_table$id[i],".csv",sep="")
  
  write.csv(datas, file = dataexportpath, row.names=FALSE)
  #output_i= data.frame(paste(id_table$id[i]),paste(id_table$group[i]), shortpath, animal, arena_type,env[[i]]$r,env[[i]]$outer_r, stimuli, env[[i]]$stripe_w,frequence_of_paint, env[[i]]$duration)
  #special correction
  output_i= data.frame(date_of_exp,paste(id_table$id[i]),paste(id_table$group[i]), shortpath, animal, arena_type,arena_size,light_source, stimuli, stimuli_type,frequence_of_paint, recording_length, resolution)
  #
  output_csv= rbind(output_csv, output_i)
}
names(output_csv)=c("date","id","genotype", "path", "animal","arena_type", "arena_size","light_source","stimuli", "stimuli_type", "frequence_of_paint","duration","pixel_size_mm" )

setwd(folderpath)
write.csv(output_csv, file="metadata.csv",row.names=FALSE)
save.image(file= paste(outputfile,"_workspace.rdata"))

#folderpath= paste(outputpath,"/dataexport", sep="")
#dir.create(folderpath)
#write.csv(output_csv)

########loading here
# 
# if (load) {
# 	
# 	message("Loading Files")
# 	pg.bar <- txtProgressBar(min = 0, max = length(fileName), initial = 0, char = "=",style = 3)
# 	
# 	# load data
# 	for (i in c(1:length(params))) {
# 		message(paste(c("loading fly ",params[[i]]$FLY,params[[i]]$DATAFILE),collapse=""))
# 		new_traj = load.buridan.data(params[[i]],99+i, CENTERHERE)
# 		if (i>1)
# 			traj <- c(traj,new_traj)
# 		else
# 			traj <- new_traj
# 		setTxtProgressBar(pg.bar, i)
# 		
# 	}
# 	
# 	close(pg.bar)
# }	
# 
# #create id_table
# id_table = data.frame(id=levels(factor(id(traj))),group)
# f_table=data.frame (id_table[,1:2])
# 
# #save this session
# save(traj,fileName,dataexportpath,group,id_table,env,file=last_session_path)
# 
# # get only five minutes
# g_5min_only= ifelse(exists("g_5min_only"),g_5min_only,FALSE)
# if (g_5min_only){
#   newtraj=c()
#   for (i in 1:nrow(id_table)){
#   currtraj =traj[id(traj)==id_table$id[i] ]
#   if (length(newtraj)==0) newtraj=c(get5min(currtraj))
#   else newtraj=c(newtraj,get5min(currtraj))
#   }
#   traj=newtraj
# }  
#   
# 
# 
# 
# message("Loading done")