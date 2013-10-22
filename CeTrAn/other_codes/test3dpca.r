library(corrplot)

message("start PCA")
#f_table= f_table_ori

f_table_new = f_table 
f_table2 =f_table_new
#colnames(f_table_new)
#
##firstpaperorder
# colnames(f_table2)=c ("AA_id","AB_group","H_speeds","N_distance_traveled","D_turning_angle","E_meander", 
                       # "O_activitytime_ST",
                     # "K_act_bouts_ST","C_pause_length_ST","I_#pauses_ST","P_activitytime_timeT",
                     # "L_act_bouts_DtimeT","B_pause_length_timeT","J_#pauses_timeT", "F_thigmotaxis_moving", "G_thigmotasix_pause","M_#walks","AC_stripe_deviation")
                     
if (MINCOL==2){
	colnames(f_table2)= c("AA_id","AB_group","H_speeds","N_distance_traveled","D_turning_angle","E_meander","XA_activitytime_ST","XB_act_bouts_ST","XC_pause_length_ST","XD_#pauses_ST","P_activitytime_timeT","L_act_bouts_DtimeT","B_pause_length_timeT","J_#pauses_timeT","F_thigmotaxis_moving", "G_thigmotasix_pause","M_#walks","AC_stripe_deviation")
}

if (MINCOL==8){
	colnames(f_table2)= c("AA_id","AB_group","ZA_genotype",  "ZB_treatment", "ZC_machine","ZD_other", "ZE_date","ZF_timeofday","H_speeds","N_distance_traveled","D_turning_angle","E_meander","XA_activitytime_ST","XB_act_bouts_ST","XC_pause_length_ST","XD_#pauses_ST","P_activitytime_timeT","L_act_bouts_DtimeT","B_pause_length_timeT","J_#pauses_timeT","F_thigmotaxis_moving", "G_thigmotasix_pause","M_#walks","AC_stripe_deviation")
}      
###add this to make the pca over the genotype group only

# if (MINCOL==8){
	# colnames(f_table2)= c("AA_id","ZA_group2","AB_group",  "ZB_treatment", "ZC_machine","ZD_other", "ZE_date","ZF_timeofday","H_speeds","N_distance_traveled","D_turning_angle","E_meander","XA_activitytime_ST","XB_act_bouts_ST","XC_pause_length_ST","XD_#pauses_ST","P_activitytime_timeT","L_act_bouts_DtimeT","B_pause_length_timeT","J_#pauses_timeT","F_thigmotaxis_moving", "G_thigmotasix_pause","M_#walks","AC_stripe_deviation")
# }                

data.frame(names(f_table),names(f_table2))
## get linearity score out
#f_table2=f_table2[,-16]

## reorder variables following the free walk analysis
f_table2= f_table2[,order(colnames(f_table2))]

### gives output if not all calculation are made
#if(length(f_table_new) != 19)  f_table2=f_table_new


#### ENTER HERE MANUALLY A NEW DATA AS ENTRY FOR THE PCA ANALYSIS
# f_table2= f_table


i_table =na.omit(f_table2)
h_table<-f_table2[,c(3:(length(f_table2)-MINCOL+2-4))] #+2 from group and id, -4 to take ST out
rownames (h_table) = paste (c(1:length(f_table2[,1])),f_table2[,2])
#h_table<-f_table2[,c(3:13,16:length(f_table2))]
g_table <- na.omit(h_table)
 head(g_table)
mydata.pca <- prcomp(g_table, retx= TRUE, center= TRUE, scale.=TRUE)



sd <- mydata.pca$sdev 
loadings <- mydata.pca$rotation 
rownames(loadings) <- colnames(g_table) 
scores <- mydata.pca$x 


t=mydata.pca$sdev^2 / sum(mydata.pca$sdev^2)
t2= cumsum(t)
plot(t2*10, main= "variance explained cumulative")


PCA_res= data.frame(scores)
PCA_res$group = as.factor(id_table$genotype)

#PCA_res$group = i_table[,19]
 
#Plot PC1 and 2 info on one graph
PCA1to3 = data.frame(PCA_res$PC1,PCA_res$PC2,PCA_res$PC3,PCA_res$group)

Mean_PCA = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),1:2)
Mean_PCA2 = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),c(1,3))  
#################################################
###################################################
##################################################
setwd(outputpath)
scalingfact=7
scalingaxis=1

 
###prepare mean tables
PCA1to3 = data.frame(PCA_res$PC1,PCA_res$PC2,PCA_res$PC3,PCA_res$group)
Mean_PCA_3d = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),1:3)
Mean_PCA = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),1:2)


abc= c(1:8, rgb(202,100,20,maxColorValue = 255),rgb(100,0,200,maxColorValue = 255))

 plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", 
   type="n", main="distance biplot",xlim=c(-max(scores[,1:2]*scalingaxis), max(scores[,1:2]*scalingaxis)), 
   ylim=c(-max(scores[,1:2]*scalingaxis), max(scores[,1:2]*scalingaxis))) 


for (i in 1:length(levels(PCA_res$group))){
  X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	points(X$PC2~ X$PC1, col=abc[i])
	#legend(list(x=-6,y=-i/2+5), legend=(levels(PCA_res$group)[i]), fill= i, bty="n")
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i,  cex=0.7)
legend("topleft", legend=(levels(PCA_res$group)[i]), fill= abc[i], bty="n", inset = c(0,i/30)) 

   x=Mean_PCA$means$PCA_res.PC1[i]
y=Mean_PCA$means$PCA_res.PC2[i]
x2=Mean_PCA$ses$PCA_res.PC1[i]
y2=Mean_PCA$ses$PCA_res.PC2[i]
segments (x-x2,y,x+x2,y, col=abc[i])
segments (x,y-y2,x,y+y2, col=abc[i])
	}
	abline (v=0, h=0)
	
 plot(scores[,1], scores[,3], xlab="PCA 1", ylab="PCA 3", 
   type="n", main="distance biplot",xlim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis)), 
   ylim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis))) 


for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	points(X$PC3~ X$PC1, col=abc[i])
	#legend(list(x=-6,y=-i/2+5), legend=(levels(PCA_res$group)[i]), fill= i, bty="n")
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i,  cex=0.7)
legend("topleft", legend=(levels(PCA_res$group)[i]), fill= abc[i], bty="n", inset = c(0,i/30)) 

   x=Mean_PCA2$means$PCA_res.PC1[i]
y=Mean_PCA2$means$PCA_res.PC3[i]
x2=Mean_PCA2$ses$PCA_res.PC1[i]
y2=Mean_PCA2$ses$PCA_res.PC3[i]
segments (x-x2,y,x+x2,y, col=abc[i])
segments (x,y-y2,x,y+y2, col=abc[i])
	}
	abline (v=0, h=0)	



##########################################
## plot data pc1 and 2 / 1 and 3 without points
M=max(Mean_PCA$means)+max(Mean_PCA$ses)*1.1
 plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M))

#plot(PCA_res$PC2~PCA_res$PC1, type= "n", add=TRUE)
for (i in 1:length(levels(PCA_res$group))){
  X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
legend("topleft", legend=(levels(PCA_res$group)[i]), fill= abc[i], bty="n", inset = c(0,i/30))
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
#   cex=0.7) 
   x=Mean_PCA$means$PCA_res.PC1[i]
y=Mean_PCA$means$PCA_res.PC2[i]
x2=Mean_PCA$ses$PCA_res.PC1[i]
y2=Mean_PCA$ses$PCA_res.PC2[i]
segments (x-x2,y,x+x2,y, col=abc[i])
segments (x,y-y2,x,y+y2, col=abc[i])
	}
	abline (v=0, h=0)
	



#######
######################################################
#plot all
require (rgl)
load("/Users/choupi/Desktop/pca3d.rdata")


setwd(outputpath)
pdf("samplesize_color_forpca.pdf")
par(mai= c(1.5,1.5,1.5,1.5))
plot(PCA_res$group, type="n", main="samplesize", ylim=c(0,30))
for (i in 1:length(levels(PCA_res$group))){
	
  plot(PCA_res$group[PCA_res$group == levels(PCA_res$group)[i]], col=abc[i], add=TRUE)
  }
 
dev.off()  
par(mai= c(1,1,1,1))

LEV=0.4
scalingfact=1

plot3d(c(0,PCA_res$PC1)*scalingfact,c(0,PCA_res$PC3)*scalingfact,c(0,PCA_res$PC2)*scalingfact,
       type = "n",box =FALSE,axes=TRUE,xlab="PC1",ylab="PC3",zlab="PC2")

  for (i in 1:length(levels(PCA_res$group))){
  #add points:
  #X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
  points= data.frame(X$PC1, X$PC3,X$PC2)
  #plot3d(points, col=i,type= "p",add=TRUE)
  


 
x=Mean_PCA_3d$means$PCA_res.PC1[i]
y=Mean_PCA_3d$means$PCA_res.PC3[i]
z=Mean_PCA_3d$means$PCA_res.PC2[i]
x2=Mean_PCA_3d$ses$PCA_res.PC1[i]
y2=Mean_PCA_3d$ses$PCA_res.PC3[i]
z2 =Mean_PCA_3d$ses$PCA_res.PC3[i] 
  A= matrix (c(x-x2,y,z,x+x2,y,z,x,y-y2,z,x,y+y2,z,x,y, z-z2,x,y,z+z2),6,3, byrow =TRUE)
  segments3d(A, col=abc[i])
  #textplot (levels(PCA_res$group)[i],col=i, add=TRUE)
  
    plot3d(ellipse3d(cov(points), centre = c(x,y,z), level=LEV), col=i, alpha=0.3 , add=TRUE)
  }  

#PCA_res$group= as.factor(id_table$genotype) 