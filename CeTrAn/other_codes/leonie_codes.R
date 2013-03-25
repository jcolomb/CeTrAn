

#########################################################
#get individual angle histograms +combined individuals
#########################################################

nflys = length(id_table$group) #number of flys
bins=seq(0,110,by=5)  #define number of degree bins
ndegs = length(bins)-1
nvals = ndegs * nflys
repna = rep(NA, nvals)

#make density_angles table with four columns full of NAs 
density_angles = data.frame(group = factor(repna), fly = factor(repna), degrees = repna, probs = repna) 
levels(density_angles$group) = levels(id_table$group) # defines as factors (words, not numbers)
levels(density_angles$fly) = levels(id_table$id) #defines as factors (words, not numbers)

#par(mfrow = c(4,5)) #makes subplots (four rows, 5 cols)
for (i in c(1:nrow(id_table))) { 
  iflys = angle_table$id == id_table$id[i] #get rows for this fly
  move = na.omit(angle_table[iflys,]$dev) #deviances without NAs
  if  (length(move)<5) { #skip the histogram if there are less than 5 moves
    histNA=0
  }else { #if there are more than 5 moves
    histA=hist(move,breaks=bins, main = id_table$id[i], xlab = 'Degree bins', ylab ='Prob.')
    irows = ((i * ndegs) - ndegs + 1) : (i*ndegs) #make rows according to length of degrees
    density_angles$degrees[irows] = histA$mids #fill 'degrees' column with middle of bin value, 'mids'
    density_angles$probs[irows] = histA$density  #fill 'probs'column with densities for each bin
    density_angles$fly[irows] = id_table$id[i]  #fill 'fly' column with fly ID
    density_angles$group[irows] = id_table$group[i]  #fill 'group' column with group ID
    }
} #end for loop



##########################################################################
### Calculate means and standard errors of angle deviations for each group
##########################################################################

###Function to calculate a standard error
sterr <- function(x){
  x = na.omit(x)
  if (length(x)>0){
    sd(x)/sqrt(length(x))
  }else{
  NA
  }
}

#make a table "ang.means" and "ang.se" with the means and standard errors for each group at each bin
midvals = histA$mids
ang.means = tapply(density_angles$probs, list(density_angles$degree, density_angles$group), mean, na.rm=T)
# applies 'mean' function (without na's) to the probability of each angle (probs) for each degree across each group
# and makes a matrix "ang.means" with the outputs
ang.se = tapply(density_angles$probs, list(density_angles$degree, density_angles$group), sterr)

##############################################
#make group plots on one graph
#############################################

groupnames = levels(id_table$group) #get names of groups
grouplev = length(levels(group))

#define number of flies in each group
groupnums = c()
for (i in c(1:grouplev)) {
groupnums[i]=  sum(id_table$group == groupnames[i])
}

dev.new() #makes new figure device
par(mfrow=c(1,1))
plotcolour = c("red","blue","green","darkmagenta","purple","orange","pink")

setwd(outputpath)
pdf('Leonie_angleplot.pdf')
#plot first graph
plot(midvals, ang.means[,1], type = 'l', xlab = 'Degree bins', ylab ='Prob.',col = plotcolour[1], ylim=c(0,max(ang.means)+.005))
lines(midvals, ang.means[,1] + ang.se[,1], lty = 2, col = plotcolour[1]) #plot standard deviations with dotted lines (lty=2)
lines(midvals, ang.means[,1] - ang.se[,1], lty = 2, col = plotcolour[1])


########################
###layer other graphs on top (if more than one group)
if (grouplev<2){dev.off()
                }else{
for(i in c(2:grouplev)) {
  lines(midvals, ang.means[,i], type = 'l', xlab = 'Degree bins', ylab ='Prob.',col = plotcolour[i], ylim=c(0,max(ang.means)+.005))
  lines(midvals, ang.means[,i] + ang.se[,i], lty = 2, col = plotcolour[i]) #plot standard deviations with dotted lines (lty=2)
  lines(midvals, ang.means[,i] - ang.se[,i], lty = 2, col = plotcolour[i])  
}
legend('topright', legend = groupnames, lty = 1, col = plotcolour, bty='n')

dev.off()
}


############################################################################
######  Calculate optomotor score as angular velocity (deg/sec) #######
############################################################################

btraj <- (bindltraj(traj)) ## bind trajectories so that each fly only has one traj
angvel = c()

for (i in c(1:nrow(id_table))) {
  
  if (sum(!is.na(btraj[[i]]$rel.angle))<20)
  {angvel[i] = NA} #if sum of angles for that traj is less than 20, put NA
  else if (params[[i]]$FLY=="a") #if anticlockwise
  {angvel[i] = mean(btraj[[i]]$rel.angle, na.rm = TRUE)*180/pi*-10} #mean angle in degrees (negative for anticlockwise)
  else{
    angvel[i] = mean(btraj[[i]]$rel.angle, na.rm = TRUE)*180/pi*10  #mean angle in degrees, x 10 (to get deg/sec as each data point in traj is for 0.1seconds)
  }
  
}


angvel_table = data.frame(group = id_table$group, angvel = angvel) # make table with angular velocities alongside group name
f_table = data.frame(f_table,angvel = angvel)
#####################################################################
setwd(rgghome)

