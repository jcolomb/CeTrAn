# takes g_inputdir,g_filetable and g_outputdir
##and g_supress_paints
# return occupancy plots 


### R CODE ###
##############
message("starting occupancy.r")


### create ocupancy plot

# get the x,y coordinates of all the individuals
xy_table = data.frame()
for (i in c(1:nrow(id_table))) {
	itraj = traj[id(traj)==id_table$id[i]]
	

#### center data 	
	x=itraj[[1]]$x - env[[i]]$cx
	y=itraj[[1]]$y - env[[i]]$cy

# delete paints when the fly is not moving
	if (g_supress_paints) {
		dx = itraj[[1]]$dx
		dy = itraj[[1]]$dy
		dx[is.na(dx)] = 0	
		dy[is.na(dy)] = 0
		new_x = c()
		new_y = c()	
		moved_distance = 0
		for (j in c(1:length(x))) {
			moved_distance = moved_distance + sqrt(dx[j]*dx[j]+dy[j]*dy[j])
			# if the fly moved 2 mm, add a point to xy_table
			if (moved_distance< g_treshold) {
				new_x = c(new_x,x[j])
				new_y = c(new_y,y[j])
				moved_distance = 0
			}
		}
	} else {
		new_x = x
		new_y = y
	}
		
		

	xy = data.frame(x=new_x,y=new_y)	
	id = rep(id_table$id[i],nrow(xy))
	group = rep(id_table$group[i],nrow(xy))
	xy_table = rbind(xy_table,data.frame(xy,id,group))
}

# paint the diagram for each group
group_names = levels(id_table$group)

# weights for smoothing
weights = c(21,16,4,1)

for (i in c(1:length(group_names))) {
	x = xy_table[xy_table$group==group_names[i],]$x
	y = xy_table[xy_table$group==group_names[i],]$y
	
	hbin = hexbin(x,y,xbins=60)
	hbin = hsmooth(hbin,wts=weights)

	#calculate the color-cuts for the plot
	maxval = quantile(hbin@count,0.95) # 95% quantil
	maxcut = maxval/max(hbin@count)
	cuts = seq(0,maxcut,length=255)
	cuts = c(cuts,1)

	plot(hbin,colorcut=cuts,colramp=blue2red,
		main=paste("Occupancy plot for",group_names[i]),legend=FALSE)
}
