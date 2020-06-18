# copied from R-bloggers, written by aschinchon
# https://www.r-bloggers.com/cellular-automata-the-beauty-of-simplicity/

library(sp)
width <-2^5
depth <- 200
gt = GridTopology(cellcentre=c(1,1),cellsize=c(1,1),cells=c(width, depth))
gt = SpatialGrid(gt)
z <- data.frame(status=sample(0:0, width, replace=T))
z[width/2, 1] <- 1
z[width/2+1, 1] <- 1
for (i in (width+1):(width*depth))
{
  ilf <- i-width-1
  iup <- i-width
  irg <- i-width+1
  if (i%%width==0) irg <- i-2*width+1
  if (i%%width==1) ilf <- i-1
  if((z[ilf,1]+z[iup,1]+z[irg,1]>0)&(z[ilf,1]+z[iup,1]+z[irg,1]<3))
  {st <- 1} else {st <- 0}
  nr<-as.data.frame(st)
  colnames(nr)<-c("status")
  z<-rbind(z,nr)
}
sgdf = SpatialGridDataFrame(gt, z)
image(sgdf, col=c("white", "black"))
