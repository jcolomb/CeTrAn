# takes fileName and datapath and retuns traj,env,umg,params
# read xml
params <- load.buridan.xml(fileName)
# vars
env <- create.env.vars(params)
#create arena polygon vector
umg <- as.area(create.arena.poly(env))
# read the data and create trajectorie
traj = load.buridan.data(params)
