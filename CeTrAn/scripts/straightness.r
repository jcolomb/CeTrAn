### compute turning angle

message ("start straightness.r")

plot_title = "Straightness Histogram (Groups)"
plot_title2 = "Straightness Histogram (Individuals)"
	
straight_table = data.frame()
median_angle = c()
median_meander = c()
	
#compute straightness for each individum
for (i in c(1:nrow(id_table))) { 
	straightnessres =c.straightness(traj[id(traj)==id_table$id[i]])
	angle = straightnessres$angle
	meander =straightnessres$meander
	#hist(abs(meander), main = id_table$id[i])
	median_angle = c(median_angle,median(abs(angle)))
	median_meander = c(median_meander,quantile(abs(meander), probs =0.5, na.rm=TRUE))
	id = rep(id_table$id[i],length(angle))
	group = rep(id_table$group[i],length(angle))
	straight_table = rbind(straight_table,data.frame(angle,meander,id,group))
}
#straight_table$angle = straight_table$angle/pi*180
#straight_table$meander = abs(straight_table$meander/pi*180)
#straight_mtable=create.mean.table (straight_table,group_ids,1:2)

#straight_table_2=data.frame(median_angle,median_meander,id=id_table$id,group=id_table$group)

f_table = data.frame(f_table,turning_angle=median_angle,meander=median_meander)

f_table_positive = data.frame(f_table_positive,turning_angle=median_angle,meander=median_meander)
#mean_table = create.mean.table(straight_table_2,group_ids,data_cols=1:2)

message("starting plots straightness")
### create plots
# M= max(mean_table$means$median_angle)+ max(mean_table$ses$median_angle)*1.3
# mybarplot(mean_table$means$median_angle,mean_table$ses$median_angle,rownames(mean_table$means),
	# main="Mean of median of turning angle",ylab="Angle [degrees]", ylim= c(0,M))

# M= max(mean_table$means$median_meander)+ max(mean_table$ses$median_meander)*1.3
# mybarplot(mean_table$means$median_meander,mean_table$ses$median_meander,rownames(mean_table$means),
	# main="Mean of median of the meander",ylab="meander [degrees*s/mm]", ylim= c(0,M))
#
#if (g_bin_size==0)
#	{g_bin_size = 0.5}
#v = straight_table$meander
#bins = seq(min(v[!is.na(v)])-g_bin_size,max(v[!is.na(v)])+g_bin_size,g_bin_size)
#
###plot histograms
##hplot1 = histogram(~ meander | group, data=straight_table, breaks = bins, type="percent",col=0,ylab="percent", xlab="relative angle [degree]", layout=c(2,2), polot = FALSE)
###hplot2 = histogram(~ angle | group, data=straight_table, scales=list(x = list(log = "e")),breaks = bins, type="percent",col=0,ylab="percent", xlab="relative angle [degree]", layout=c(2,2))	
###hplot2 = histogram(~ angle | id ,data=straight_table, breaks = bins, type="percent",col=0,ylab="percent", xlab="relative angle [degree]", layout=c(3,3))
##print(hplot1, xlim=c(0,90))
###print(hplot2

