#initialisation
times=c()

##get time/date
load.time <- function(par, id_prefix) {
  
 
  
  experiment_time = as.numeric(par$TIMESTAMP)	
  fly_id <- paste(id_prefix,par$FLY,sep="_")
  
  #create time
  timeatstart <- ISOdatetime(1970,1,1,0,0,0) + experiment_time
  
  
 xx= as.POSIXct(strftime(timeatstart, format="%H:%M:%S"), format="%H:%M:%S")
  res=data.frame(id=fly_id,date=as.Date(timeatstart),timeofday=xx)
  
  return(res)
    
}
# load data
	for (i in c(1:length(params))) {
		#message(paste(c("loading fly ",params[[i]]$FLY,params[[i]]$DATAFILE),collapse=""))
		
		new_traj = load.time(params[[i]],99+i)#, CENTERHERE=T)
		if (i>1){
			times <- rbind(times,new_traj)
		}else{
			times <- new_traj
		}	
		#setTxtProgressBar(pg.bar, i)
		
	}
	
	#times
##get other group management

#if (file.exists(XXX))

groupdetailfile="Groupdetail.csv"
setwd(g_inputdir)
if (file.exists(groupdetailfile)){
	groupdl= read.csv(groupdetailfile)
	groupdl <- data.frame(lapply(groupdl, as.character), stringsAsFactors=FALSE)

	# id_table$genotype=id_table$group
	# id_table$treatment=id_table$group
	# id_table$machine=id_table$group
	# id_table$other=id_table$group
	# levels (id_table$genotype)= levels(groupdl$genotype)
	# levels (id_table$treatment)= levels(groupdl$treatment)
	# levels (id_table$machine)= levels(groupdl$machine)
	# levels (id_table$other)= levels(groupdl$other)
	
	for (i in c(1:nrow(id_table))){
			id_table$genotype[i]= groupdl$genotype[groupdl$Original_group==as.character(id_table$group[i])]
			id_table$treatment[i]= groupdl$treatment[groupdl$Original_group==as.character(id_table$group[i])]
			id_table$machine[i]= groupdl$machine[groupdl$Original_group==as.character(id_table$group[i])]
				id_table$other[i]= groupdl$other[groupdl$Original_group==as.character(id_table$group[i])]	
		}
	}else{message("no detail group indication, only the group name is given.")
	}	
	#levels(id_table$group)

###include new data into f_table, f_tablepositive and f_table_index



###specific for CS data
if (ncol(id_table)>3){
  id_table[,2]= as.factor(paste(id_table[,3],id_table[,6], sep="_"))
  
}
#######
group= id_table[,2]

setwd(rgghome)

	

