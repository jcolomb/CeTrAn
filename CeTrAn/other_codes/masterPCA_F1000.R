###
x=c("xmlfile", "group")
grouping = rbind (c("CSbvs_1.xml" ,   "BB_CSBS"),c("CSbvs_2.xml" ,"BB_CSBS"))
colnames(grouping)=x
#names(f_table)[2]="group"
#previousdata=f_table
#result_table=f_table
### grouping should be produced by your uploading code, you should make sure the "group" name was not already given by previous users. this is only for testing purpose
##data$number_of_walks=data$number_of_walks/data$to_divide
g_pca= FALSE
g_occupancy=FALSE
g_filetablename = grouping

source ("CeTrAn_norgg_xml.r") ### you will need to check if some values need to be changed at the beginning of the code, in addition to g_pca, do not run *pdfoutput.r* the output of this code is "result_table" which is written in output_table.csv normally

previousdata= read.csv ("allrawdata.csv") #need to check what happens for the first run when no "allrawdata.csv" exist

newdata= rbind(previousdata,result_table) # here we merge the results for the novel analysis with the data previously uploaded and analysed

write.csv(newdata, "allrawdata.csv")

data= newdata
source ("other_codes/PCA_stablef1000.R")# you need to get line 7 out

## this value "abc" is deciding the color of the plot. it could eventually be chosen by the user, that is why I put the plotting code apart: if we make this value changeable, we do not have to do the PCA analysis again.

abc= c(2,rep(1,length(levels(PCA_res$group))-1)) #last group in red, all the other in black
#abc=c (1:600) # color used per default, only 6 different colors used in loop

source ("other_codes/plotting_PCA_f1000.R") # you will need to change *pdf("test.pdf")* (l.9) with the appropriate output, probably *png("pca_plot.png")*

##there you should get an image.
## users should be able to see the image and download the "allrawdata.csv" file.

#I have tested the second part of the code using output_csv files that I uploaded independently, I can modify the code once you would show me the upload function and what you get out of it.
