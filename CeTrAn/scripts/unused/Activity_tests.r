# takes g_inputdir,g_filetable and g_outputdir
##and g_duration_slider, g_bin_size
# return activity plots


message("starting activity.r")
### compute the activities
	
act_table = data.frame()

#compute activities for each individum
for (i in c(1:nrow(id_table))) {
	act = c.activity(traj[id(traj)==id_table$id[i]],g_duration_slider/10)
	act_id = rep(id_table$id[i],nrow(act))
	act_group = rep(id_table$group[i],nrow(act))
	act_table = rbind(act_table,data.frame(act,id=act_id,group=act_group))

}

# filter activities which are smaller than the act slider
pause_table = act_table[act_table$act<0,]
pause_table$pause = - pause_table$act
act_table = act_table[act_table$act>0,]

#calculate total activity time for each individual
sum_act = c()
median_act = c()
median_pause = c()
number_pause = c()
for (i in c(1:nrow(id_table))) {
	act = act_table[act_table$id==id_table$id[i],]$act
	pause = pause_table[act_table$id==id_table$id[i],]$act
	sum_act = c(sum_act,sum(act))
	median_act = c(median_act,median(act))
	median_pause = c(median_pause, median(abs(pause)))
	number_pause = c(number_pause,length(pause))
	
}
act_table_2 = data.frame(sum_act,median_act,median_pause,number_pause,id=id_table$id,group=id_table$group)

message("starting writing activities.txt")



setwd(rgghome)

# create bins
if (g_bin_size==0)
	g_bin_size = 0.5		
v = act_table$act
bins = seq(min(v[!is.na(v)])-g_bin_size,max(v[!is.na(v)])+g_bin_size,g_bin_size)

#create mean and sd table
mean_table = create.mean.table(act_table_2,group_ids,data_cols=1:4)


message("starting plots activity")
### create plots
mybarplot(mean_table$means$sum_act,mean_table$ses$sum_act,rownames(mean_table$means),
	main="Total activity time",ylab="Total activity time [s]")
mybarplot(mean_table$means$median_act,mean_table$ses$median_act,rownames(mean_table$means),
	main="Mean activity time",ylab="Median activity time [s]")

mybarplot(mean_table$means$number_pause,mean_table$ses$number_pause,rownames(mean_table$means),
	main="Number of pauses",ylab="Number of pauses")

mybarplot(mean_table$means$median_pause,mean_table$ses$median_pause,rownames(mean_table$means),
	main="Mean pause time",ylab="Median pause time [s]")

message("starting activity frequence plot")
hplot2 = histogram(~ act | group ,data=act_table,breaks=bins-0.01, type="percent",col=0)
hplot2 = update(hplot2,main="Activity Histogram (Groups)", ylab="frequency", xlab="activity time [s]",layout=c(2,2))
print(hplot2)





hplot2 = hist(pause_table$pause,breaks= c(seq(0,200, 0.05),  seq(220,1000,20)), plot=FALSE)
print(plot(hplot2, xlim=c(0,100)))

#hplot2$counts = (hplot2$counts/ length (pause_table$pause))
print(plot(hplot2, xlim=c(0,100)))


hplot2$counts[hplot2$counts==0] <- -1


hplot2$counts = log(hplot2$counts)
#hplot2$counts[is.na(hplot2$counts)] <- -1


print(plot(hplot2$counts, xlim=c(0,1000), pch=20, xlab = "pause length", ylab= "ln(counts)"))

