# ##set the working directory to this file parent folder
# rgghome="~/Gits/CeTrAn/CeTrAn"
# 
# GRvar=2 ## say which variable to use at the grouping variable while producing graphs, group=2, genotype=3,...
# ## this is to be modified according to data location

# outputfile = "output"
# g_inputdir = "~/Gits/Buridan_5CS/4CS_data"
# g_outputdir="~/Gits/Buridan_5CS/4CS_data/output"
# g_filetablename ="diffCS_all.txt"

##those are the variables
# g_duration_slider = 10  #default=10, min0 max20
# g_bin_size= 1 #default=1, min0 max20
# g_supress_paints=TRUE
# g_treshold = 8 # min0 max20
# 
# g_general= TRUE
# g_roundarena= TRUE
# g_stripes_deviation=TRUE
# g_occupancy= TRUE
# g_angledev= TRUE
# g_outputtext= TRUE
# g_log= TRUE
# g_thigmo= TRUE
# g_pca= TRUE
# g_individual= TRUE
# g_open_pdf=FALSE

###reading the table of groups
# g_filetable= read.csv(paste(g_inputdir,g_filetablename, sep="/"),sep = "\t", header=FALSE)


### R CODE ###
##############
# setwd(rgghome)

message("start")
# load libs and functions
source("functions/include.r", local=TRUE)

#check if input is ok
if (!exists("g_inputdir")||!exists("g_filetable"))
  stop("No input files specified.")

if (ncol(g_filetable)!=2)
	stop("Input matrix has a wrong format")

# determine path
datapath = g_inputdir

message(datapath)

# set output dir, need to create the directory if not existant
outputpath = ""
if (exists("g_outputdir")) {
	outputpath = g_outputdir
} else {
  dir.create (paste(c(datapath,"/output"),collapse=""))
	outputpath = paste(c(datapath,"/output"),collapse="")	
}



##fordebug
message("1st")
setwd(outputpath)
save.image(file= paste(outputfile,"_workspace.rdata"))
setwd(rgghome)
message("2nd")

#load fly data
fileName = g_filetable[,1]
group = g_filetable[,2]

# takes fileName, datapath and group
source("scripts/MultipleFilesLoader.r", local=TRUE)
# "traj" contains the trajectories, "env[[i]]" the area informations
# "params[[i]]" the Experiment Parameters and Informations
# id_table the animals and corresponding groups

# print trajectorie information
#print(traj)

# get group and individum information


### compute and plot data


#suppressWarnings(dir.create(outputpath))


###########
#########


#individual traces first#

#setwd(rgghome)



# load libs and functions again to avoid any old function to be present
#source("functions/include.r")
if (g_individual) {
	source ("scripts/singleflyoutputs.r", local=TRUE)}

source ("scripts/expand_f_table.r", local=TRUE)

group_ids = levels(factor(id_table$group))

print(outputpath)
# write to
message("second")
setwd(outputpath)
message(paste(c("Writing output to ",outputpath),collapse=""))

bla = paste(c(outputfile,"t",g_treshold,"Pt",g_duration_slider,".pdf"),collapse="")
pdf(bla)


setwd(rgghome)
source ("scripts/samplesize.r", local=TRUE)
source ("scripts/exp_length.r", local=TRUE)
if (g_general) {
	source ("scripts/general.r", local=TRUE)
	source ("scripts/straightness.r", local=TRUE)
	
	
	source ("scripts/activity_martin.r", local=TRUE)

	source ("scripts/activity_log.r", local=TRUE)

	
}

if (g_roundarena) {
source ("scripts/thigmotaxis.r", local=TRUE)
}

if (g_stripes_deviation){
source ("scripts/angledev.r", local=TRUE)

}
source ("scripts/saveworkspace.r", local=TRUE)
#

setwd(rgghome)
source ("scripts/changef_table.r", local=TRUE)
source ("scripts/pdfoutput.r", local=TRUE)


if (g_occupancy) {
	source ("scripts/occupancy.r", local=TRUE)
	}


source ("scripts/saveworkspace.r", local=TRUE)

if (g_pca) {source ("scripts/pca.r", local=TRUE)
	}

source ("scripts/saveworkspace2.r", local=TRUE)


	
# close write
g = dev.off()







# 
# # open pdf
# setwd(outputpath)
# plot_path = paste(c(getwd(),"/",bla),collapse="")
# if (g_open_pdf) {
# 	if (.Platform$OS.type=="Linux")
# 		system(paste(c("gnome-open",plot_path),collapse=" "))
# 	else
# 		system(paste(c("open",plot_path),collapse=" "))
# }
# 
# 
# 
# 
# 
# 
