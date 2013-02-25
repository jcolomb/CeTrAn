# straightness: calculate linearity of the trajectory for each bouts = distance premier dernier point/dist totale

tr <- as.traj(id = id, xy = xy, date = da)

sp<-speed(tr)

disuc<-sp$speed*sp$dt
ditot<-tapply(disuc, sp$id, sum)

xyperan<-split(xy, id)


dd<-function(x) {
        xfirst<-x[1,]
        xend<-x[nrow(x),]
        di<-sqrt((xfirst[1]-xend[1])^2 +
            (xfirst[2]-xend[2])^2)
        return(di)
}
dilin<-unlist(lapply(xyperan, dd))