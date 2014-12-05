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
setwd(rgghome)


# load libs and functions
source("functions/include.r")

#check if input is ok
if (is.na(g_inputdir)||is.na(g_filetable))
	stop("No input files specified.")

if (ncol(g_filetable)!=2)
	stop("Input matrix has a wrong format")

# determine path
datapath = g_inputdir

# set output dir, need to create the directory if not existant
outputpath = ""
if (!is.na(g_outputdir)) {
	outputpath = g_outputdir
} else {
  dir.create (paste(c(datapath,"/output"),collapse=""))
	outputpath = paste(c(datapath,"/output"),collapse="")	
}

##fordebug
setwd(outputpath)
save.image(file= paste(outputfile,"_workspace.rdata"))
setwd(rgghome)

#load fly data
fileName = g_filetable[,1]
group = g_filetable[,2]

# takes fileName, datapath and group
source("scripts/MultipleFilesLoader.r")
# "traj" contains the trajectories, "env[[i]]" the area informations
# "params[[i]]" the Experiment Parameters and Informations
# id_table the animals and corresponding groups

# print trajectorie information
#print(traj)

# get group and individum information


### compute and plot data


suppressWarnings(dir.create(outputpath))


###########
#########


#individual traces first#

setwd(rgghome)



# load libs and functions again to avoid any old function to be present
#source("functions/include.r")
if (g_individual) {
	source ("scripts/singleflyoutputs.r")}

source ("scripts/expand_f_table.r")

group_ids = levels(factor(id_table$group))

# write to
setwd(outputpath)
message(paste(c("Writing output to ",outputpath),collapse=""))

bla = paste(c(outputfile,"t",g_treshold,"Pt",g_duration_slider,".pdf"),collapse="")
pdf(bla)


setwd(rgghome)
source ("scripts/samplesize.r")
source ("scripts/exp_length.r")
if (g_general) {
	source ("scripts/general.r")
	source ("scripts/straightness.r")
	
	
	source ("scripts/activity_martin.r")

	source ("scripts/activity_log.r")

	
}

if (g_roundarena) {
source ("scripts/thigmotaxis.r")
}

if (g_stripes_deviation){
source ("scripts/angledev.r")

}
source ("scripts/saveworkspace.r")
#

setwd(rgghome)
source ("scripts/changef_table.r")
source ("scripts/pdfoutput.r")


if (g_occupancy) {
	source ("scripts/occupancy.r")
	}


source ("scripts/saveworkspace.r")

if (g_pca) {source ("scripts/pca.r")
	}

source ("scripts/saveworkspace2.r")


	
# close write
g = dev.off()








# open pdf
setwd(outputpath)
plot_path = paste(c(getwd(),"/",bla),collapse="")
if (g_open_pdf) {
	if (.Platform$OS.type=="Linux")
		system(paste(c("gnome-open",plot_path),collapse=" "))
	else
		system(paste(c("open",plot_path),collapse=" "))
}






