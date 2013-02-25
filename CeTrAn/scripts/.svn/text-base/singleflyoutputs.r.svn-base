
#g_speed_average=10



setwd(outputpath)
pdf("individual_tests.pdf")
for (i in c(1:nrow(id_table))) {
  plot.circle(traj[id(traj)==id_table$id[i]],env[[i]],g_traj_color1=T)
  #plotindividtests(traj[id(traj)==id_table$id[i]],g_speed_average=10)
}
for (i in c(1:nrow(id_table))) {
  #plot.circle(traj[id(traj)==id_table$id[i]],env[[i]],g_traj_color1=TRUE)
  plotindividtests(traj[id(traj)==id_table$id[i]],g_speed_average=10)
}


dev.off()
setwd(rgghome)

plot.circle =function (traj, env, g_traj_color1 = TRUE) 
{
  traj_title = paste(c("Trajectorie for ", id(traj[1])), collapse = "")
  if (!inherits(env, "bur.env")) 
    stop("env should be of class \"bur.env\".")
  if (!inherits(traj, "ltraj")) 
    stop("ltraj should be of class \"ltraj\".")
  umg <- as.area(create.arena.poly(env, outer_circle = FALSE))
  radius = env$r
  xdim = c((env$cx - radius), (env$cx + radius))
  ydim = c((env$cy - radius), (env$cy + radius))
  s = create.stripe.poly(env)
  plot.ltraj(traj, area = umg, xlab = "", ylab = "y [mm]", 
             main = traj_title, xlim = xdim, ylim = ydim, colpol = "white", 
             addpoints = FALSE, perani = TRUE, final = FALSE, frame = FALSE, 
             xaxt = "n")
  lines(s$x[s$p_id == 0], s$y[s$p_id == 0], lwd = 4)
  lines(s$x[s$p_id == 1], s$y[s$p_id == 1], lwd = 4)
  walks = c.nwalks(traj, env)
  if (g_traj_color1) {
    if (nrow(walks) > 0) 
      for (i in c(1:nrow(walks))) {
        print( walks$start[i])
        print (traj)
        lt = gdltraj(traj, min = walks$start[i], max = walks$end[i], 
                     type = "POSIXct")
        print(lt)
        t = ltraj2traj(lt)
        print(t)
        lines(t$x, t$y, col = 2 + i%%7)
      }
  }
}

gdltraj = function (x, min, max, type = c("POSIXct", "sec", "min", "hour", 
                                "mday", "mon", "year", "wday", "yday")) 
{
  if (!inherits(x, "ltraj")) 
    stop("x should be of class \"ltraj\"")
  if (!attr(x, "typeII")) 
    stop("x should be of type II (time recorded)")
  type <- match.arg(type)
  if (type == "POSIXct") {
    x <- lapply(x, function(y) y[(y$date > min) & (y$date < 
      max), ])
  }
  else {
    x <- lapply(x, function(y) {
      da <- as.POSIXlt(y$date)[[type]]
      return(y[(da >= min) & (da < max), ])
    })
  }
  if (all(sapply(x, nrow) == 0)) 
    stop("No relocations within the specified interval")
  x[sapply(x, nrow) == 0] <- NULL
  class(x) <- c("ltraj", "list")
  attr(x, "typeII") <- TRUE
  attr(x, "regular") <- is.regular(x)
  return(x)
}