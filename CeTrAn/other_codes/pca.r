
scalingfact=70
scalingaxis=6

#f_table= f_table_ori
f_table_ori = f_table

#f_table = f_table[1:(length(f_table)-9)]
h_table<-f_table[,c(3:(length(f_table)))]
rownames (h_table) = paste (c(1:length(f_table[,1])),f_table[,2])
#h_table<-f_table[,c(3:13,16:length(f_table))]
g_table <- na.omit(h_table)
 head(g_table)
mydata.pca <- prcomp(g_table, retx= TRUE, center= TRUE, scale.=TRUE)



sd <- mydata.pca$sdev 
loadings <- mydata.pca$rotation 
rownames(loadings) <- colnames(g_table) 
scores <- mydata.pca$x 

R <- cor(g_table) 
myEig <- eigen(R) 
sdLONG <- sqrt(myEig$values)
loadingsLONG <- myEig$vectors 
rownames(loadingsLONG) <- colnames(g_table) 
 standardize <- function(x) {(x - mean(x))/sd(x)} 
 X <- apply(g_table, MARGIN=2, FUN=standardize) 
# transforming data to zero mean and unit variance 
 scoresLONG <- X %*% loadingsLONG 
# calculating scores from eigenanalysis results 

 range(sd - sdLONG) 
 range(loadings - loadingsLONG) 
 range(scores - scoresLONG) 
 




pdf("PCA.pdf")



#Plot PC1 and 2 info on one graph
PCA1to3 = data.frame(PCA_res$PC1,PCA_res$PC2,PCA_res$PC3,PCA_res$group)

Mean_PCA = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),1:2)

Mean_PCA2 = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),c(1,3))	

 plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", 
   type="n", main="distance biplot",xlim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis)), 
   ylim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis))) 
   arrows(0,0,loadings[,1]*scalingfact,loadings[,2]*scalingfact, length=0.1, 
   angle=20, col="red") 
# note that this scaling factor of 10 may need to be changed, 
# depending on the data set 
 text(loadings[,1]*scalingfact*1.2,loadings[,2]*scalingfact*1.2, 
   rownames(loadings), col="red", cex=0.7) 
# 1.2 scaling insures that labels are plotted just beyond 
# the arrows 
# text(scores[,1],scores[,2], rownames(scores), col="blue", 
#   cex=0.7) 






plot(PCA_res$PC2~PCA_res$PC1, type= "n")
for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	points(X$PC2~ X$PC1, col=i)
	legend(list(x=-6,y=-i/2+3), legend=(levels(PCA_res$group)[i]), fill= i, bty="n")
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
#   cex=0.7) 
   x=Mean_PCA$means$PCA_res.PC1[i]
y=Mean_PCA$means$PCA_res.PC2[i]
x2=Mean_PCA$ses$PCA_res.PC1[i]
y2=Mean_PCA$ses$PCA_res.PC2[i]
segments (x-x2,y,x+x2,y, col=i)
segments (x,y-y2,x,y+y2, col=i)
	}
	abline (v=0, h=0)
	
 plot(scores[,1], scores[,3], xlab="PCA 1", ylab="PCA 3", 
   type="n", main="distance biplot",xlim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis)), 
   ylim=c(-max(scores[,1:3]*scalingaxis), max(scores[,1:3]*scalingaxis))) 
 arrows(0,0,loadings[,1]*scalingfact,loadings[,3]*scalingfact, length=0.1, 
   angle=20, col="red") 
# note that this scaling factor of 10 may need to be changed, 
# depending on the data set 
 text(loadings[,1]*scalingfact*1.2,loadings[,3]*scalingfact*1.2, 
   rownames(loadings), col="red", cex=0.7) 
# 1.2 scaling insures that labels are plotted just beyond 
# the arrows 
# text(scores[,1],scores[,2], rownames(scores), col="blue", 
#   cex=0.7) 


plot(PCA_res$PC3~PCA_res$PC1, type= "n")
for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	points(X$PC3~ X$PC1, col=i)
	legend(list(x=-6,y=-i/2+4), legend=(levels(PCA_res$group)[i]), fill= i, bty="n")
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
#   cex=0.7) 
   x=Mean_PCA2$means$PCA_res.PC1[i]
y=Mean_PCA2$means$PCA_res.PC3[i]
x2=Mean_PCA2$ses$PCA_res.PC1[i]
y2=Mean_PCA2$ses$PCA_res.PC3[i]
segments (x-x2,y,x+x2,y, col=i)
segments (x,y-y2,x,y+y2, col=i)
	}
	abline (v=0, h=0)	

dev.off()

