grouping= "group"  #PCA result grouped by variable "grouping"

setwd("/Users/colombj/Gits/CeTrAn/CeTrAn/")
source("functions/include.r")

message("start PCA")
#data=read.csv("output _table.csv")
name=c("id",grouping,"median_speed","distance_traveled_mm__permin","turning_angle",
"meander","activitytime_permin_TT","act_bouts_TT","pause_length_TT",
"numb_pauses_permin_TT","centrophobism_moving","centrophobism_sitting",      "number_of_walks_permin","stripe_deviation")

name2=c("id",grouping,"median_speed","distance_traveled_mm_p_min","turning_angle",
"meander","activitytime_TT","act_bouts_TT","pause_length_TT",
"numb_pauses_TT","centrophobism_moving","centrophobism_sitting",      "number_of_walks","stripe_deviation")

#f_table= data[,name2]
f_table= data[,name]

i_table =na.omit(f_table)
h_table<-f_table[,c(3:(length(f_table)))] #+2 from group and id, -4 to take ST out
rownames (h_table) = paste (c(1:length(f_table[,1])),f_table[,2])
g_table <- na.omit(h_table)

mydata.pca <- prcomp(g_table, retx= TRUE, center= TRUE, scale.=TRUE)


sd <- mydata.pca$sdev 
loadings <- mydata.pca$rotation 
rownames(loadings) <- colnames(g_table) 
scores <- mydata.pca$x 


#t=mydata.pca$sdev^2 / sum(mydata.pca$sdev^2)
#t2= cumsum(t)
#plot(t2*10, main= "variance explained cumulative")


PCA_res= data.frame(scores)
PCA_res$group = as.factor(i_table[,2])
PCA_res$lab=NA
PCA_res$genotype=NA


for (i in c(1: length(PCA_res$lab))){
  PCA_res$lab[i]= as.character(importdata$uploader_initials [importdata$group_name == PCA_res$group[i]])
  PCA_res$genotype[i]= as.character(importdata$fly_genotype [importdata$group_name == PCA_res$group[i]])
}
PCA_res$lab=as.factor(PCA_res$lab)
PCA_res$genotype=as.factor(PCA_res$genotype)

PCA_res$lty=as.numeric(PCA_res$genotype)
PCA_res$col=as.numeric(PCA_res$lab)



PCA1to3 = data.frame(PCA_res$PC1,PCA_res$PC2,PCA_res$PC3,PCA_res$group)
Mean_PCA_3d = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),1:3)


### variables


####
	