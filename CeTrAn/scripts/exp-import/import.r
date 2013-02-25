
#####
## load metadata via RGG 
## load input (workingdirectory) via RGG
## load include.r
## load g_treshold from RGG
## load group from RGG
#rgghome="C:/Users/colomb/Desktop/buridan_onsvn/export-importtools"

#g_inputdir="C:/Users/colomb/Desktop/buridan_onsvn/export-importtools/scripts/exp-import/testexport"
#metadatafile="fly_test/metadata.csv"
#######################################################
### begin debug part
# metadata= read.csv("D:/dokumente/data/buridan/corrected_data/tests_export/test/metadata.csv")
#input = "scripts/exp-import/testexport"
#setwd(input)
#metadata= read.csv("fly_test/metadata.csv", header=T)
#group = metadata$genotype
#g_treshold=8

#metadata$date_of_exp= as.character(ISOdatetime(1970,1,1,0,0,0) + as.numeric(params[[i]]$TIMESTAMP))
#metadata$date_of_exp= as.character(ISOdatetime(1970,1,1,0,0,0) )
########
#end debug part
setwd(g_inputdir)
metadata= read.csv(metadatafile, header=T)
group = metadata$group
#######################################################
setwd(rgghome)
A="scripts/exp-import/default.xml"

#A="D:/dokumente/data/buridan/corrected_data/schmale_streifen/cstpw1.xml"

B=load.buridan.xml(A)



import.param <-function (flyname, metadata, A="scripts/exp-import/default.xml"){
    
l=metadata[metadata$id==flyname, ,drop=TRUE]

l= drop.levels(l)
l
  doc <- xmlInternalTreeParse(A)
  data <- xmlChildren(xmlChildren(doc)$HEADER)
  values <- sapply(data, xmlValue)
	erg <- lapply(values,unlist)
	
    erg$FLY = as.character(l$id)
    erg$DATAFILE = as.character(l$path)
    erg$DURATION = as.character(l$duration)
    erg$STRIPE_WIDTH = as.character(l$stimuli_type)
    erg$STRIPE_POS # no change, work only with -90,90 for now
    erg$ARENA_DIAMETER_MM = as.character(l$arena_size *2)
    erg$OUTER_DIAMETER_MM = as.character(l$light_source)
    erg$ARENA_CENTER_X = 0
    erg$ARENA_CENTER_Y = 0
    erg$ARENA_RADIUS = as.character(l$arena_size)
  #timecalc
  y=as.POSIXct (l$date)
  Timeofexp=as.numeric(y)- as.numeric(ISOdatetime(1970,1,1,0,0,0))
  ###
    erg$TIMESTAMP = Timeofexp
    erg$RESX ="640" # no change
    erg$RESY = "480" # no change
    erg$COMMENTS = as.character(l$date_of_exp)


	#datapath = strsplit(A,"/")	
	#datapath = paste(c(datapath[[1]][1:length(datapath[[1]])-1],""),collapse="/")
	erg$DATAPATH = "not used"	
	
	class(erg) <- "bur.xml"
return(erg)
}

flynumb=c(1: nrow(metadata))
flyname= as.character(metadata$id[flynumb])

params2 <- sapply(flyname, function(name) {
  list(import.param(flyname=name,metadata=metadata))
})


env <- lapply(params2, create.env.vars.center)


fileName = metadata$path


load=T
setwd(g_inputdir)

if (load) {
	
	message("Loading Files")
	pg.bar <- txtProgressBar(min = 0, max = length(fileName), initial = 0, char = "=",style = 3)
	
	# load data
	for (i in c(1:length(params2))) {
		message(paste(c("loading fly ",params2[[i]]$FLY,params2[[i]]$DATAFILE),collapse=""))
		new_traj = load.buridan.data.fmeta(params2[[i]],99+i, center=F)
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

save.image(file= paste(outputfile,"_datainput.rdata"))
message("Loading done")
