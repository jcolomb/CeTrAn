

PCA_res= data.frame(scores)
class(PCA_res)
PCA_res$PC1
i_table =na.omit(f_table)
PCA_res$group = i_table[,2]


#Plot PC1 and 2 info on one graph
PCA1_2 = data.frame(PCA_res$PC1,PCA_res$PC2,PCA_res$group)
PCA1_2
Mean_PCA = create.mean.table(PCA1_2,levels(PCA1_2$PCA_res.group),1:2)	

plot.new()
plot(PCA_res$PC2~PCA_res$PC1, type= "n")
for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	points(X$PC2~ X$PC1, col=i)
	legend(list(x=6,y=-i/2-1), legend=(levels(PCA_res$group)[i]), fill= i, bty="n")
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



#Plot only the mean and ses

x=Mean_PCA$means$PCA_res.PC1
y=Mean_PCA$means$PCA_res.PC2
x2=Mean_PCA$ses$PCA_res.PC1
y2=Mean_PCA$ses$PCA_res.PC2

plot(Mean_PCA$means$PCA_res.PC1,Mean_PCA$means$PCA_res.PC2, ylim= c(-2,2),xlim= c(-2,2), type="n")
i=1:6
 text(x-0.05,y-0.05, rownames(Mean_PCA$means), col=i, 
   cex=0.7) 
segments (x-x2,y,x+x2,y, col=i)
segments (x,y-y2,x,y+y2, col=i)

x=Mean_PCA2$means$PCA_res.PC1
y=Mean_PCA2$means$PCA_res.PC3
x2=Mean_PCA2$ses$PCA_res.PC1
y2=Mean_PCA2$ses$PCA_res.PC3

plot(Mean_PCA2$means$PCA_res.PC1,Mean_PCA2$means$PCA_res.PC3, ylim= c(-2,2),xlim= c(-2,2), type="n")
i=1:6
 text(x-0.05,y-0.05, rownames(Mean_PCA2$means), col=i, 
   cex=0.7) 

segments (x-x2,y,x+x2,y, col=i)
segments (x,y-y2,x,y+y2, col=i)

A=aov(PCA_res$PC1~PCA_res$group)
A=aov(X$PC1~ X$genotype*X$treatment)
summary(A)
A=aov(X$PC2~ X$genotype*X$treatment)
summary(A)
A=aov(PCA_res$PC1~PCA_res$group)
A=aov(Y$PC1~ Y$genotype*Y$treatment)
summary(A)
A=aov(Y$PC2~ Y$genotype*Y$treatment)
summary(A)

A=aov(Y$PC3~ Y$genotype*Y$treatment)
summary(A)
A=aov(X$PC3~ X$genotype*X$treatment)
summary(A)

Y=X
X=subset (X, X$genotype !="rut1")
 X=subset (X, X$genotype !="rut2080")
 X=subset (X, X$genotype !="dnc")
X= droplevels(X)
