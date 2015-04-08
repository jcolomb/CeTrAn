### code for figure 4 in the F1000 paper


REBOOT =F#if true: all data are recalculated
only_one_additional_group=F #if true, groups selected such that only the group given will be analysed with the 5 initial groups.

#Additional_group = "data entered by the user!"

#indicate analysis variables:
g_duration_slider = 10  #default=10, min0 max20
g_bin_size= 1 #default=1, min0 max20
g_supress_paints=TRUE
g_treshold = 8 # min0 max20

#indicate which analysis is done
g_general= TRUE
g_roundarena= TRUE
g_stripes_deviation=TRUE
g_occupancy= TRUE
g_angledev= TRUE
g_outputtext= TRUE
g_log= TRUE
g_thigmo= TRUE
g_pca= FALSE
g_individual= FALSE
g_open_pdf=FALSE


outputfile = "temp/output"

##NEED TO BE CHANGED WHEN MOVING TO A NEW ENVIRONMENT
#define folders where files are read/written
g_inputdir = "~/Desktop/F1000/Buridan_data_reworked/uploads"
g_outputdir="~/Desktop/F1000/Buridan_data_reworked/output"
rgghome= "~/Gits/CeTrAn/CeTrAn"
##



setwd(g_inputdir)
#uploads.csv is modified after the data is uploaded via the F1000 code
importdata= read.csv("uploads.csv")

#import the previous analysis


if (REBOOT) {onlycolname=structure(list(id = logical(0), group = logical(0), date = logical(0), 
                                        timeofday = logical(0), length_experiment = logical(0), median_speed = logical(0), 
                                        distance_traveled_mm__permin = logical(0), turning_angle = logical(0), 
                                        meander = logical(0), activitytime_permin_ST = logical(0), 
                                        act_bouts_ST = logical(0), pause_duration_ST = logical(0), 
                                        numb_pause_permin_ST = logical(0), activitytime_permin_TT = logical(0), 
                                        act_bouts_TT = logical(0), pause_length_TT = logical(0), 
                                        numb_pauses_permin_TT = logical(0), centrophobism_moving = logical(0), 
                                        centrophobism_sitting = logical(0), number_of_walks_permin = logical(0), 
                                        stripe_deviation = logical(0), UID = logical(0)), .Names = c("id", 
                                                                                                     "group", "date", "timeofday", "length_experiment", "median_speed", 
                                                                                                     "distance_traveled_mm__permin", "turning_angle", "meander", "activitytime_permin_ST", 
                                                                                                     "act_bouts_ST", "pause_duration_ST", "numb_pause_permin_ST", 
                                                                                                     "activitytime_permin_TT", "act_bouts_TT", "pause_length_TT", 
                                                                                                     "numb_pauses_permin_TT", "centrophobism_moving", "centrophobism_sitting", 
                                                                                                     "number_of_walks_permin", "stripe_deviation", "UID"), class = "data.frame", row.names = integer(0))
             setwd(g_outputdir)  
             write.csv(onlycolname,"analysed_data.csv")              
}
  analyseresults=read.csv(paste(g_outputdir,"analysed_data.csv", sep="/"))
  analyseresults=analyseresults[,-1]
  analyseresults$UID= as.factor(analyseresults$UID)
  analysed = levels(analyseresults$UID)


# check for new data to analyse, analyse it and add it to the result table

for (i in c(1:nrow(importdata))){
  UID=importdata[i,1]
  if (!UID %in% analysed){
    message("YES")
    if (exists("combf_table")){analyseresults=combf_table}
    groupname=importdata[i,8]
    g_filetablename = paste (groupname, "/grouping.csv",sep="")
    g_filetable= read.csv(paste(g_inputdir,g_filetablename, sep="/"),sep = ",", header=FALSE)
    g_filetable[,1]= paste(groupname,g_filetable[,1], sep="/")
    setwd(rgghome)  
    source("CeTrAn_norgg_xml.r")
    setwd(g_outputdir)
    f_table$UID = UID
    combf_table= rbind( f_table,analyseresults)
    
    #write.csv(f_table,"analysed_data.csv")
    
  }
setwd(g_outputdir)  
### if new data write the new analysis, if no new data, just write the data into the combf_table variable
if (exists("combf_table")){
  write.csv(combf_table,"analysed_data.csv")
  }else {combf_table=analyseresults}

}

#now the result table is used to perform the PCA:
data=combf_table
setwd(rgghome)
if (only_one_additional_group){
  source(other_codes/f1000_only_one_group.r)
}

source ("other_codes/PCA_stablef1000.R")# you need to get line 7 out

##not in use anymore: old way to add colors 
##this value "abc" is deciding the color of the plot. it could eventually be chosen by the user, that is why I put the plotting code apart: if we make this value changeable, we do not have to do the PCA analysis again.

#abc= c(2,rep(1,length(levels(PCA_res$group))-1)) #last group in red, all the other in black
#abc=c (1:600) # color used per default, only 6 different colors used in loop

source ("other_codes/plotting_PCA_f1000.R") # you will need to change *pdf("test.pdf")* (l.9) with the appropriate output, probably *png("pca_plot.png")*

##there you should get an image.
## users should be able to see the image and download the "allrawdata.csv" file.


