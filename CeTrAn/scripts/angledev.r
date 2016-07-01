message("starting angledev")

### number of walks between stripes
#compute walks for each individum
walks = c()
for (i in c(1:nrow(id_table))) {
  walks = c(walks,nrow(c.nwalks(traj[id(traj)==id_table$id[i]],env[[i]])))
}
walks=walks/f_table$length_experiment

f_table = data.frame(f_table,number_of_walks_permin=walks)

f_table_positive= data.frame(f_table_positive,number_of_walks_permin=walks)


# takes g_inputdir,g_filetable and g_outputdir
# takes g_bin_size
# return angle deviation = stripe deviation plots (mean of median, histogram /not the individuals)





### compute angle deviation

#plot_title = "Stripe Deviation Histogram (Groups)"
#plot_title2 = "Stripe Deviation Histogram (Individuals)"
	
angle_table = data.frame()

#compute angle deviations for each individum
for (i in c(1:nrow(id_table))) {	
	
	angle = c.angle.dev(traj[id(traj)==id_table$id[i]],env[[i]])
	id = rep(id_table$id[i],nrow(angle))
	group = rep(id_table$group[i],nrow(angle))
	angle_table = rbind(angle_table,data.frame(angle,id,group))
}


# create bins
# if (g_bin_size==0)
	# g_bin_size = 0.5		
# v = angle_table$dev
# bins = seq(min(v[!is.na(v)])-g_bin_size,max(v[!is.na(v)])+g_bin_size,g_bin_size)


# compute median of the deviation

median_dev = c()
for (i in c(1:nrow(id_table))) {

	dev_2 = angle_table[angle_table$id==id_table$id[i],]$dev

	median_dev = c(median_dev,median(dev_2,na.rm = TRUE))

	
}
angle_table_2 = data.frame(median_dev,id=id_table$id,group=id_table$group)
f_table=data.frame(f_table,stripe_deviation=median_dev)
f_table_index = data.frame(f_table_index,stripe_deviation=median_dev)
#create mean and sd table

# mean_table = create.mean.table(angle_table_2,group_ids,data_cols=1:1)


# message("starting plots mean")
# mybarplot(mean_table$means$median_dev,mean_table$ses$median_dev,rownames(mean_table$means),
	# main="mean of medians of stripe deviation",ylab="angle deviation [degrees]", ylim = c(0,ylim_angledev))
  # abline(h=45, col=2)



#create two histograms
#hplot1 = histogram(~ dev | group ,data=angle_table, breaks=bins-0.01, type="percent",col=0, include.lowest= TRUE)	
##hplot2 = histogram(~ dev | id ,data=angle_table,breaks=bins, type="percent",col=0)
#hplot1 = update(hplot1,main=plot_title, ylab="Frequency", xlab="Angle deviation [degree]",layout=c(2,2))
##hplot2 = update(hplot2,main=plot_title2, ylab="Frequency", xlab="Angle deviation [degree]",layout=c(3,3))
#print(hplot1)
##print(hplot2)
#
#

