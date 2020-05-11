library(diagram)
opar <- par()
par(ask=TRUE)
par(mfrow=c(1,1))
figNr <- 1
subtitle <- function()
{
  mtext(side=1,outer=TRUE,"Marine foodweb modeling by Hafez Ahmad  ",cex=0.7,adj=1,line=-1.5)
  mtext(side=1,outer=TRUE,paste("  Figure:",figNr,sep=""),cex=0.7,adj=0,line=-1.5)
  figNr <<- figNr +1
}


par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
names <- c("Phytoplankton",expression(NH[4]^"+"),"Zooplankton","Detritus","Bottom_detritus","Fishes")
M <- matrix(nrow=6,ncol=6,byrow=TRUE,data=c(
  #   p n z  d  b  f
  0,1,0, 0, 0, 0, #p
  0,0,4, 10,11,0, #n
  2,0,0, 0, 0, 0, #z
  8,0,13,0, 0, 12,#d
  9,0,0, 7, 0, 0, #b
  0,0,5, 0, 0, 0  #f
))
pp<-plotmat(M,pos=c(1,2,1,2),curve=0,name=names,lwd=1,my=0.,cex.txt=0.8,
            box.lwd=2,box.size=0.08,box.type="square",box.prop=0.5,
            arr.type="triangle",arr.pos=0.4,shadow.size=0.01,prefix="f")

# extra arrows: flow 5 to Detritus and flow 2 to detritus
phyto   <-pp$comp[1,]
zoo     <-pp$comp[3,]
nh3     <-pp$comp[2,]
detritus<-pp$comp[4,]
fish    <-pp$comp[6,]

# flow6->detritus
m2 <- 0.5*(zoo+fish)
m1 <- detritus
m1[1]<-m1[1]+ pp$radii[4,1]
mid<-straightarrow (from=m2,to=m1,arr.type="triangle",arr.pos=0.4,lwd=1)
text(mid[1],mid[2]+0.03,"f6",cex=0.8)

# flow3->detritus
m2 <- 0.5*(zoo+phyto)
m1 <- detritus
m1[1] <-m1[1]+ pp$radii[3,1]*0.2
m1[2]<-m1[2] + pp$radii[3,2]
mid<-straightarrow (from=m2,to=m1,arr.type="triangle",arr.pos=0.3,lwd=1)
text(mid[1]-0.01,mid[2]+0.03,"f3",cex=0.8)

# solar radiation
m1 <- 0.5*(nh3+phyto)
m2 <- c(0.25,0.9)
segments (m1[1],m1[2],m2[1],m2[2],lwd=1,lty=2)
text(m2[1]-0.01,m2[2]+0.03,"solar radiation",adj=c(0.5,0.5))

# chlorophyll
m1 <- phyto
m1[1] <-m1[1]+ pp$radii[1,1]
m2 <- m1
m2[1]<-m2[1]+0.25
segments (m1[1],m1[2],m2[1],m2[2],lwd=1)
textellipse(mid=m2,radx=pp$radii[1,1],rady=pp$radii[1,2],lwd=1,shadow.size=0,lab="Chlorophyll")
subtitle()





library(deSolve)
#y<-rN(1-N/k)
r<-1;k<-10;init<-2
derivs<-function(t,N,params)
  list(r*N*(1-N/k))


times<-seq(0,20,0.2)
times
out<-ode(y=init,times=times,func=derivs,parms=NULL)
plot(out)
matplot(out)

init<-12
out1<-ode(y=init,times=times,func=derivs,parms=NULL)

init<-6
out3<-ode(y=init,times=times,func=derivs,parms=NULL)
init<-20
out4<-ode(y=init,times=times,func=derivs,parms=NULL)


plot(out,out1,out3,out4,main='logistic population growth simulation',lwd=2,col=c("#FF8C00", "#1E90FF", "#8B7D6B", "#00EE00"))
legend("bottomright", legend=c('inital condition=2','int.=12','ini.=5','int.=20'),
       col=c("#FF8C00", "#1E90FF", "#8B7D6B", "#00EE00"), bg="transparent",lty=1:2, cex=0.8)




