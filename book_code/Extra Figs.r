#----------------------------------- Extra Figs --------------------------------#

Fig.1<-function(){
library(cluster)
data(plantTraits)
barplot(plantTraits[,1][3:8],names.arg = rownames(plantTraits)[3:8],ylab="Diaspore Mass (mg)",xlab="Plant Speices",col=rainbow(20))
}

################################################

Fig.2<-function(){
library(boot)
data(melanoma)
Male.live<-length(melanoma$time[melanoma$sex==1&melanoma$status==1])
Male.die<-length(melanoma$time[melanoma$sex==1&melanoma$status==2])
Female.die<-length(melanoma$time[melanoma$sex==0&melanoma$status==2])
Female.live<-length(melanoma$time[melanoma$sex==0&melanoma$status==1])
Fem<-c(Female.die,Female.live)
Male<-c(Male.die,Male.live)
mel<-rbind(Male,Fem)
colnames(mel)<-c("Dead","Alive")
barplot(mel,legend.text=c("Male","Female"),ylab="Count",xlab="Outcomes for patients with malignant melanoma")
} 

##################################################

Fig.3<-function(){
require(hdrcde) 
require(vioplot) 
require(Hmisc) 
x <- c(rnorm(100,0,1), rnorm(100,3,1)) 
par(mfrow=c(1,5), mar=c(3,2,4,1)) 
xxx <- seq(min(x), max(x), length=500) 
yyy <- dnorm(xxx)/2 + dnorm(xxx, mean=3)/2 
plot(yyy, xxx, type="l",main="Underlying\ndensity") 
mtext("(a)",3,at=-.005)
boxplot(x, col="gray90", main="standard\nboxplot") 
mtext("(b)",3,at=0.3)
hdr.boxplot(x, main="HDR\nboxplot") 
mtext("(c)",3,at=0.3)
vioplot(x)
title("violin plot")
mtext("(d)",3,at=0.3)
bpplot(x,main="Box-percentile\nplot")
mtext("(e)",3,at=-.7)
}

###################################################

Fig.4<-function(){
data(Glucose2)
attach(Glucose2)
plot(Time,glucose,xlab="Time after alchohol ingestion (min/10)",ylab="Blood glucose conc. (ml/dl)",type="n")
lines(Time[Subject==1&Date==1],glucose[Subject==1&Date==1],col=gray(0),lty=1, lwd = 2)
lines(Time[Subject==2&Date==1],glucose[Subject==2&Date==1],col=gray(0),lty=2, lwd = 2)
lines(Time[Subject==3&Date==1],glucose[Subject==3&Date==1],col=gray(0),lty=3, lwd = 2) 
lines(Time[Subject==4&Date==1],glucose[Subject==4&Date==1],col=gray(0),lty=4,lwd = 2)
lines(Time[Subject==5&Date==1],glucose[Subject==5&Date==1],col=gray(0.5),lty=1, lwd = 2)
lines(Time[Subject==6&Date==1],glucose[Subject==6&Date==1],col=gray(0.5),lty=2, lwd = 2)
lines(Time[Subject==7&Date==1],glucose[Subject==7&Date==1],col=gray(0.5),lty=3, lwd = 2)
legend("topright",legend=seq(1,7),col=c(rep(1,4),rep("gray",3)),lty=c(1,2,3,4,1,2,3),title="Subject",xpd=.5, lwd = 2)
}

####################################################

Fig.5<-function(){
library(cluster)
data(plantTraits)
stars(plantTraits[1:7,][,1:10],key.loc=c(6,2.2),col.segment=rainbow(20),draw.segments = TRUE)
}

####################################################

Fig.6<-function(){ 
library(climatol)
precip<-t(c(93.42,71.87,79.02,77.89,82.50,68.77,48.54,34.43,45.34,53.72,82.79,94.07))
##t means transpose.  These are monthly averages of precip in mm.
max.t<-t(c(5.43,-2.61,-5.54,-4.76,-2.92,1.22,5.32,9.25,13.21,18.32,17.61,12.06))
## Monthly average maximum temp in degrees C
min.t<-t(c(-13.74,-13.78,-11.30,-8.38,-3.90,-0.13,3.41,3.04,-0.66,-4.96,-11.33,-13.99))
## Monthly average minimum temp in degrees C
abs.min.t<-t(c(-15.87,-15.43,-14.52,-11.30,-8.61,-1.57,1.78,1.17,-3.26,-9.09,-14.43,-16.04))
##Lowest observed temp in degrees C  
Beartooth<-rbind(precip,max.t,min.t,abs.min.t)
colnames(Beartooth)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
diagwl(Beartooth,mlab="en",alt=2853,per="[19 15]",est="Beartooth Lake, WY, USA, Lat 44.9N, Long 109.6W")
}

#####################################################

Fig.7<-function(){
library(plotrix)
sand<-c(70.00,72.00,68.00,69.00,66.00,68.50,96.00,72.00,65.00,65.00,74.00,71.50)
silt<-c(29.00,26.00,28.00,30.00,34.00,28.50,4.00,24.00,34.00,29.00,25.00,28.50)
clay<-c(1.00,2.00,4.00,1.00,0.00,3.00,0.00,4.00,1.00,6.00,1.00,0.00)
Wash1<-cbind(sand,silt,clay)
soil.texture(Wash1,pch=19,col.symbols=2)
}

#####################################################

Fig.8<-function(){
library(lattice)
data(starkey)
sk<-starkey
wireframe(xlim=c(374400,380400),ylim=c(5007000,5015700),as.numeric(sk[,11])~as.numeric(sk[,2])*as.numeric(sk[,3]),aspect = c(0.88, 0.12),light.source = c(20,40,5),pretty=T,shade=T,zlab="Elev.",ylab="Northing",xlab="Easting",cex=.9)
}


