#report the lenght of the experiment for each fly
times = c()
for (i in c(1:nrow(id_table))) {
	tim = c.time(traj[id(traj)==id_table$id[i]])
	times = c(times,tim)
}
f_table = data.frame(f_table,length_experiment=times)
