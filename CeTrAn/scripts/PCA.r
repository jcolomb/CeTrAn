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
PCA_res$group = as.factor(i_table[,2])

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
	
 plot(scores[,1], scores[,3], xlab="PCA 1", ylab="PCA 3", 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M)) 


for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC3~ X$PC1, col=i)
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

################
############### plot loadings
M= max (loadings[,1:3])*1.25
 plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M))

for (k in c(1: nrow(loadings))){   
  arrows(0,0,loadings[k,1],loadings[k,2], length=0.1, 
  angle=20, col=k) 
# note that this scaling factor of 10 may need to be changed, 
# depending on the data set 
  text(loadings[k,1]*1.2,loadings[k,2]*1.2, 
  rownames(loadings)[k], col=k, cex=0.7)
}
  
# 1.2 scaling insures that labels are plotted just beyond 
# the arrows 
# text(scores[,1],scores[,2], rownames(scores), col="blue", 
#   cex=0.7) 

#plot(scores[,1], scores[,3], xlab="PCA 1", ylab="PCA 3", 
 #   type="n", main=paste ("distance biplot",levels((f_table$group))[i] ),xlim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis)), 
  #    ylim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis))) 
     
 plot(scores[,1], scores[,3], xlab="PCA 1", ylab="PCA 3", 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M))

for (k in c(1: nrow(loadings))){   
  arrows(0,0,loadings[k,1],loadings[k,3], length=0.1, 
  angle=20, col=k) 
# note that this scaling factor of 10 may need to be changed, 
# depending on the data set 
  text(loadings[k,1]*1.2,loadings[k,3]*1.2, 
  rownames(loadings)[k], col=k, cex=0.7) 
}




###########
########## correlation matrices under is all correlation, up only the significant one (p< 0.05)

cor.prob3 <- function(X, dfr = nrow(X) - 2) {
   R <- cor(X)
   T=R
   
   above <- row(R) < col(R)
   r2 <- R[above]^2
   Fstat <- r2 * dfr / (1 - r2)
	 R[above] <-  pf(Fstat, 1, dfr)
   G = R[above]
   i = c(1:length(R[above]))
    G[i]=ifelse(R[above][i]>0.95 ,1,0)
    G[i]=ifelse(R[above][i]>0.99 ,1,G[i])
   R[above][i] =G[i]*T[above][i]
  
	 R
}



f_table2 =na.omit(f_table2)
#f_table2$AB_group =f_table2$group
for(i in 1:length(levels((f_table2$AB_group)))){
  totest= f_table2[f_table2$AB_group==levels((f_table2$AB_group))[i],]
  totestdata= totest[,c(3:(length(totest)))]
  
  CORRMATRIX= cor.prob3 (totestdata)
  
  #corrplot.number(CORRMATRIX, title=paste(levels((f_table2$AB_group))[i], "n=", nrow(totest)), diag= 
  #  FALSE)
  #col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white",
#"cyan", "#007FFF", "blue","#00007F"))
  col3 <- colorRampPalette(c("green", "white", "red"))
  
  corrplot(CORRMATRIX, method = "circle",title=paste(levels((f_table2$AB_group))[i], "n=", nrow(totest)), diag= FALSE, col=col3(30) )
  
#   corrplot.circle(CORRMATRIX, title=paste(levels((f_table$group))[i]), diag= 
#    FALSE)
  #CORRMATRIX2= cor (g_table[A:B,])
 # corrplot.circle(CORRMATRIX, title=paste("stripes",levels((f_table$group))[i]), order="alphabet")

}
###
###other calculations
# 
# R <- cor(g_table) 
# myEig <- eigen(R) 
# sdLONG <- sqrt(myEig$values)
# loadingsLONG <- myEig$vectors 
# rownames(loadingsLONG) <- colnames(g_table) 
#  standardize <- function(x) {(x - mean(x))/sd(x)} 
#  X <- apply(g_table, MARGIN=2, FUN=standardize) 
# # transforming data to zero mean and unit variance 
#  scoresLONG <- X %*% loadingsLONG 
# # calculating scores from eigenanalysis results 
# 
#  range(sd - sdLONG) 
#  range(loadings - loadingsLONG) 
#  range(scores - scoresLONG) 
# 
# scalingfact=7.5
# scalingaxis=1
# plot.new()
#  plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", 
#    type="n", main="distance biplot",xlim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis)), 
#    ylim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis))) 
#    arrows(0,0,loadings[,1]*scalingfact,loadings[,2]*scalingfact, length=0.1, 
#    angle=20, col="red") 
# # note that this scaling factor of 10 may need to be changed, 
# # depending on the data set 
#  text(loadings[,1]*scalingfact*1.2,loadings[,2]*scalingfact*1.2, 
#    rownames(loadings), col="red", cex=0.7) 
# 1.2 scaling insures that labels are plotted just beyond 
# the arrows 
# text(scores[,1],scores[,2], rownames(scores), col="blue", 
#   cex=0.7) 

# 
#  plot(scores[,1], scores[,3], xlab="PCA 1", ylab="PCA 3", 
#    type="n", main="distance biplot",xlim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis)), 
#    ylim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis))) 
#  arrows(0,0,loadings[,1]*scalingfact,loadings[,3]*scalingfact, length=0.1, 
#    angle=20, col="red") 
# # note that this scaling factor of 10 may need to be changed, 
# # depending on the data set 
#  text(loadings[,1]*scalingfact*1.2,loadings[,3]*scalingfact*1.2, 
#    rownames(loadings), col="red", cex=0.7) 
# # 1.2 scaling insures that labels are plotted just beyond 
# # the arrows 
# # text(scores[,1],scores[,2], rownames(scores), col="blue", 
# #   cex=0.7) 
# 
# 
# 
# # biplot(scores[,1:2], loadings[,1:2], xlab=rownames(scores), 
#   # ylab=rownames(loadings), cex=0.7, main="distance biplot using built-in biplot function") 
# # using built-in biplot function : not working here??
# 
# #correlation biplot
# 
# #  plot(scores[,1]/sd[1], scores[,3]/sd[3], xlab="PCA 1", 
# #    ylab="PCA 3", type="n", main ="correlation biplot") 
# #  arrows(0,0,loadings[,1]*sd[1],loadings[,3]*sd[3], 
# # 
# #    length=0.1, angle=20, col="red") 
# #  text(loadings[,1]*sd[1]*1.2,loadings[,3]*sd[3]*1.2, 
# #    rownames(loadings), col="red", cex=0.7) 
# #  text(scores[,1]/sd[1],scores[,3]/sd[3], rownames(scores), 
# #    col="blue", cex=0.7) 
# # # 1.2 scaling insures that labels are plotted just beyond 
# # # the arrows 
# #   plot(scores[,2]/sd[2], scores[,3]/sd[3], xlab="PCA 2", 
# #    ylab="PCA 3", type="n", main ="correlation biplot") 
# #  arrows(0,0,loadings[,2]*sd[2],loadings[,3]*sd[3], 
# # 
# #    length=0.1, angle=20, col="red") 
# #  text(loadings[,2]*sd[2]*1.2,loadings[,3]*sd[3]*1.2, 
# #    rownames(loadings), col="red", cex=0.7) 
# #  text(scores[,2]/sd[2],scores[,3]/sd[3], rownames(scores), 
# #    col="blue", cex=0.7) 
# # # 1.2 scaling insures that labels are plotted just beyond 
# # # the arrows 
# # 
# #  plot(scores[,1]/sd[1], scores[,2]/sd[2], xlab="PCA 1", 
# #    ylab="PCA 2", type="n", main ="correlation biplot") 
# #  arrows(0,0,loadings[,1]*sd[1],loadings[,2]*sd[2], 
# # 
# #    length=0.1, angle=20, col="red") 
# #  text(loadings[,1]*sd[1]*1.2,loadings[,2]*sd[2]*1.2, 
# #    rownames(loadings), col="red", cex=0.7) 
# #  text(scores[,1]/sd[1],scores[,2]/sd[2], rownames(scores), 
# #    col="blue", cex=0.7) 
# # # 1.2 scaling insures that labels are plotted just beyond 
# # # the arrows 
# 
# 
#  biplot(mydata.pca, main ="using biplot pcaresult") 
#  
#  #analysis
# correlations <- cor(scores,g_table) 
# 
# 
# 
# plot(log(sd^2), xlab="principle component", 
#    ylab="log(variance)", type="b", pch=16) 
# # using a general plot, with variance on a log scale 
# 
#  newsd <- sd(scores) 
#  max (sd - newsd) 
# # finds maximum difference between the standard deviation form 
# # prcomp and the standard deviation calculated longhand; 
# # should be close to zero 
#  eigenvalues <- sd^2 
#  sum(eigenvalues)  
# # should equal number of variables 
# length(g_table)  
# # number of variables 
# 
# 
# 
# 
# PCA_res= data.frame(scores)
# PCA_res$group = i_table[,2]
# class(PCA_res)
# PCA_res$PC1
# i_table =na.omit(f_table2)
# PCA_res$group = i_table[,2]
# 
# cor.prob <- function(X, dfr = nrow(X) - 2) {
#    R <- cor(X)
#    above <- row(R) < col(R)
#    r2 <- R[above]^2
# 	 Fstat <- r2 * dfr / (1 - r2)
# 	 R[above] <-   pf(Fstat, 1, dfr)
# 	 R
# }
# cor.prob2 <- function(X, dfr = nrow(X) - 2) {
#    R <- cor(X)
#    T=R
#    
# 	 above <- row(R) < col(R)
# 	 r2 <- R[above]^2
# 	 Fstat <- r2 * dfr / (1 - r2)
# 	 R[above] <-  pf(Fstat, 1, dfr)
#    G = R[above]
#    i = c(1:length(R[above]))
#     G[i]=ifelse(R[above][i]>0.95 ,-0.5,0)
#     G[i]=ifelse(R[above][i]>0.99 ,1,G[i])
#    R[above] =G
# 	 R
# }
# library(corrplot)
        setwd(rgghome)