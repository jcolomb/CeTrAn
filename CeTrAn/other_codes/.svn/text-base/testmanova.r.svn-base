require(heplots)
require(candisc)
setwd(outputpath)

l_table= subset(f_table,f_table$group != "freewalk_CS")
l_table= subset(f_table,f_table$group != "20d_CS")
l_table= subset(f_table,f_table$group != "11d_CS")
l_table= drop.levels(l_table)
#l_table$group
#l_table= f_table
#l_table= f_table2
i_table <- na.omit(l_table)
g_table<-i_table[,c(3:(length(i_table)))]

gen <-i_table[,2]
h=as.matrix(g_table)
m1= manova(h~gen)
plot(h~gen)
summary(m1)
summary.aov(m1)
class(m1)

heplot3d(m1)
pairs(m1)
####

m2 <- lm(h~gen)

can1= candisc(m1)
can2 = candisc(m2)

heplot(can1, prefix = "canonical dimension", main = "canonical HE plot")








#####
h=as.matrix(PCA_res[,1:22])
gen= PCA_res[,23]


h_table<-f_table[,c(3:(length(f_table)))]
g_table <- na.omit(h_table)
g=scale(as.matrix(g_table))
fit <- kmeans(g, 4)
library(cluster) 
clusplot(h, as.numeric(gen), color=TRUE, shade=TRUE,
   labels=2, lines=0)
clusplot(, fit$cluster, color=TRUE, shade=TRUE,
   labels=2, lines=0)
   
   library(fpc)
plotcluster(g, na.omit(f_table)[,2],,method="wnc",clnum=2) 
clusplot(g, na.omit(f_table)[,2])