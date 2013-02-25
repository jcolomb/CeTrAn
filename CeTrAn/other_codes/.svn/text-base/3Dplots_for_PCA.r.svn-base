###PCA3d on a data frame names f_table2, without the two first columns used for grouping
### with one column named "group"
for (i in 1: nrow(f_table)){
  f_table$sum[i]=sum(f_table[i,4:14])
}

require(rgl)
#####PCA
create.mean.table <- function(data_table,groups,data_cols=1,group_col=ncol(data_table)) {  
  mean_table = data.frame()
	sd_table = data.frame()
	se_table = data.frame()
	for (g in groups) {
		data = data_table[data_table[group_col]==g,]
		mean_table = rbind(mean_table,data.frame(t(colMeans(data[data_cols],na.rm=TRUE))))
		sd_table = rbind(sd_table,data.frame(t(sd(data[data_cols],na.rm=TRUE))))
		se_table = sd_table/sqrt(nrow(data))
	}
	rownames(mean_table) = groups
	rownames(sd_table) = groups
	
	erg <- list(mean_table,sd_table,se_table)
	names(erg) <- c("means","sds","ses")
	return(erg)
}
##


h_table<-f_table2[,c(3:(length(f_table2)))]
rownames (h_table) = paste (c(1:length(f_table2[,1])),f_table2[,2])
#h_table<-f_table2[,c(3:13,16:length(f_table2))]
g_table <- na.omit(h_table)
 head(g_table)
mydata.pca <- prcomp(g_table, retx= TRUE, center= TRUE, scale.=TRUE)
sd <- mydata.pca$sdev 
loadings <- mydata.pca$rotation 
rownames(loadings) <- colnames(g_table) 
scores <- mydata.pca$x



PCA_res= data.frame(scores)
PCA_res$group = i_table[,2]




PCA1to3 = data.frame(PCA_res$PC1,PCA_res$PC2,PCA_res$PC3,PCA_res$group)
Mean_PCA_3d = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),1:3)

scalingfact=15
###
### Plot variables plot in 3D (arrow plot)

plot3d(loadings[,1]*scalingfact,loadings[,3]*scalingfact,loadings[,2]*scalingfact,
       type = "n",box =FALSE,axes=TRUE,xlab="PC1",ylab="PC3",zlab="PC2")

for (j in 1:nrow(loadings)){
A=matrix (c(0,0,0,loadings[j,1]*scalingfact,loadings[j,3]*scalingfact,loadings[j,2]*scalingfact),2,3,byrow =TRUE)
segments3d(A, col=j)

text3d(loadings[j,1]*scalingfact*1.1,loadings[j,3]*scalingfact*1.1,loadings[j,2]*scalingfact*1.1, 
     rownames(loadings)[j], col=j+1, cex=0.7, add=TRUE)
}


###
### Plot data, mean +- ses on the 3 axes, and ellipse of X=20% confidence interval
## beware: no legend
LEV=0.7
plot3d(loadings[,1]*scalingfact,loadings[,3]*scalingfact,loadings[,2]*scalingfact,
       type = "n",box =FALSE,axes=TRUE,xlab="PC1",ylab="PC3",zlab="PC2")

  for (i in 1:length(levels(PCA_res$group))){
  X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
  points= data.frame(X$PC1, X$PC3,X$PC2)
  plot3d(points, col=i,type= "p",add=TRUE)
  #plot3d(X$PC1, X$PC3,X$PC2, col=i,type= "p",add=TRUE)
  #Ell= sd(points)/sqrt(nrow(points))
  plot3d(ellipse3d(cor(points), centre = mean(points), level=LEV), col=i, alpha=0.3 , add=TRUE)
  
  plot3d(ellipse3d(cov(points), centre = mean(points), level=LEV), col=i+2, alpha=0.3 , add=TRUE)
 
x=Mean_PCA_3d$means$PCA_res.PC1[i]
y=Mean_PCA_3d$means$PCA_res.PC3[i]
z=Mean_PCA_3d$means$PCA_res.PC2[i]
x2=Mean_PCA_3d$ses$PCA_res.PC1[i]
y2=Mean_PCA_3d$ses$PCA_res.PC3[i]
z2 =Mean_PCA_3d$ses$PCA_res.PC3[i] 
  A= matrix (c(x-x2,y,z,x+x2,y,z,x,y-y2,z,x,y+y2,z,x,y, z-z2,x,y,z+z2),6,3, byrow =TRUE)
  segments3d(A, col=i)
  }  

 # text3d(max(points[1]),max(points[3]),max(points[2]), 
 #    levels(PCA_res$group)[i], col=i, cex=0.7, add=TRUE)
 

#################
###################
#################
########## code to move the visualisation
 rgl.viewpoint(theta = 0, phi = 0, fov = 45, zoom = 0.8)

########## code for making a snapshot


######## multiple snapshot to transform into a video
for (i in 0:180) {
   rgl.viewpoint(theta = -180+2*i, phi =-90+(i ), fov = 45, zoom = 1)
   filename <- paste("pic1_",1000+i,".png",sep="") 
   
  #rgl.snapshot(filename, fmt="png")
}
for (k in 0:90) {
  
   rgl.viewpoint(theta = 0, phi =k, fov = 45, zoom = 1)
   filename <- paste("pic2_",1000+k,".png",sep="") 
  #rgl.snapshot(filename, fmt="png")
                     }



# F=c()
# for (g in 1: nrow(loadings)) F=c(F,0)
# 
# loadings2= rbind(loadings,F)