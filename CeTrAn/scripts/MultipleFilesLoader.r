# takes fileName,group and datapath and retuns traj,env,params,group,id_table
setwd(outputpath)
save.image(file= paste(outputfile,"_datainput.rdata"))
setwd(rgghome)
#init vars
load <- TRUE
traj <- c()
CENTERHERE <- TRUE

#load xmls
params <- sapply(fileName, function(name) {
	list(load.buridan.xml(paste(datapath,name,sep="/")))
})

#create env objects containing the arena data

if (CENTERHERE) {
	env <- lapply(params, create.env.vars.center)
	}else {env <- lapply(params, create.env.vars)}


# check if a session for these objects allready exists
sessname = paste (g_treshold,".last_session.RData",sep="_")
last_session_path <- paste(outputpath,sessname,sep="/")
if (file.exists(last_session_path)) {
	nenv <- new.env(parent = baseenv())
	load(last_session_path,nenv) 

	if (length(nenv$fileName)==length(fileName)&&all(nenv$fileName==fileName)&&nenv$datapath==datapath&&all(as.character(nenv$group)==as.character(group))) {
		load(last_session_path,.GlobalEnv)
		message("Trajectorie restored from previous Session")
		load <- FALSE
	}
	rm(nenv)
}
	
if (load) {
	
	message("Loading Files")
	pg.bar <- txtProgressBar(min = 0, max = length(fileName), initial = 0, char = "=",style = 3)
	
	# load data
	for (i in c(1:length(params))) {
		message(paste(c("loading fly ",params[[i]]$FLY,params[[i]]$DATAFILE),collapse=""))
		new_traj = load.buridan.data(params[[i]],99+i, CENTERHERE)
		if (i>1)
			traj <- c(traj,new_traj)
		else
			traj <- new_traj
		setTxtProgressBar(pg.bar, i)
		
	}
	
	close(pg.bar)
}	

#create id_table
id_table = data.frame(id=levels(factor(id(traj))),group)
f_table=data.frame (id_table[,1:2])
f_table_index=data.frame (id_table[,1:2])
f_table_positive=data.frame (id_table[,1:2])

#save this session#
#save.image(file= paste(outputfile,"_datainput.rdata"))
save(traj,fileName,datapath,group,id_table,env,file=last_session_path)
# get only five minutes
g_5min_only= ifelse(exists("g_5min_only"),g_5min_only,FALSE)
if (g_5min_only){
  newtraj=c()
  erase=c()
  for (i in 1:nrow(id_table)){
  currtraj =traj[id(traj)==id_table$id[i] ]
  traj_i= get5min(currtraj)
  
  if (is.null(traj_i)){erase=c(erase,i)
                       
    }else{if (length(newtraj)==0) {newtraj= c(traj_i)
      }else {newtraj= c(newtraj,traj_i)}
      }
  
  
  }
  traj=newtraj
  env= env[-erase]
  id_table = id_table[-erase,]
  f_table= f_table[-erase,]
  group= group[-erase]
  fileName=fileName[-erase]
  #id_table=drop.levels(na.omit(id_table))
  #row.names(id_table)= c(1: nrow(id_table))
  } 

##

##



message("Loading done")
message (group)