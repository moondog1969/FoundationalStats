#Triangle in math appendix -- Note -- Dropped from manuscript

#Right Angle Functions

plot(seq(1,20),seq(1,20),xaxt="n",yaxt="n",xlab="",ylab="",type="n",bty="l")
points(15,15,col=4)
segments(15,0,15,15,col=4,lty=2)
segments(0,0,15,15,col=4,lty=2)
segments(0,0,0,15,col=4,lty=2)
text(17,15.5,expression("(x,y)"),cex=1.75)
text(7.5,9,expression("r"),cex=1.75)
segments(15,2,13,2,col=4)
segments(13,0,13,2,col=4)
y1<-seq(pi/4,pi/2,.1)
x1<-seq(pi/4,pi/2,.1)
lines(sin(x1)*3,cos(y1)*3,col=4)
text(4,2,expression(theta),cex=1.75)



#############################################
Fig.A.1<-function(){
message("Fig.2.1 not created using R")}

#############################################


Fig.A.2<-function(){
plot(seq(1,20),seq(1,20),pch=seq(1,20),col=seq(1,20),ylab = "Symbol number", xlab = "Color number")}

#############################################

Fig.A.4<-function(){
layout(matrix(seq(1,6),3,2))
par(mar=c(1,1,1,1))
pie(rep(1,12), col=rainbow(12),main="Rainbow colors")
pie(rep(1,12), col=heat.colors(12),main="Heat colors")
pie(rep(1,12), col=topo.colors(12) ,main="Topographic colors")
pie(rep(1,12), col=gray(seq(0,1,1/12)),main="Gray colors")
pie(rep(1,12), col=hcl(h=seq(180,0,length=12)),main="Colors using hcl arguments 1")
pie(rep(1,12), col=hcl(h=seq(360,180,length=12)),main="Colors using hcl arguments 2")
}

##############################################

Fig.A.5<-function(){
data(Loblolly)
attach(Loblolly)
plot(height,age)
}

#############################################


Fig.A.6<-function(){
data(Loblolly)
attach(Loblolly)
plot(height,age,pch=2,col=3)
abline(0.7573804, 0.3782742)
}

########### Fig A.10 ####################################

library(asbio) # loads the library asbio
data(C.isotope)
dev.new(height = 8, width = 7)
op <- par(mfrow = c(2, 2), oma = c(0.1, 0.1, 0, 0), mar = c(4, 4.4, 2,
2), bg = gray(.97))
#-------------------------------- plot 1 -------------------------------#
with(C.isotope, plot(Decimal.date, D14C, xlab = "Date", ylab =
expression(paste(delta^14,"C (per mille)")), type = "n"))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
colors()[181])
with(C.isotope, points(Decimal.date, D14C, pch = 21, bg = "white"))
#-------------------------------- plot 2 -------------------------------#
with(C.isotope, plot(Decimal.date, CO2, xlab = "Date", ylab =
expression(paste(CO[2]," (ppm)")), type = "n"))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
colors()[363])
with(C.isotope, points(Decimal.date, CO2, type = "l"))
#-------------------------------- plot 3 -------------------------------#
with(C.isotope, plot(CO2, D14C, xlab = expression(paste(CO[2], "(ppm)")),
ylab = expression(paste(delta^14,"C (per mille)")), type = "n"))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
colors()[580])
with(C.isotope, points(CO2, D14C, pch = 21, bg = "yellow"))
#-------------------------------- plot 4 -------------------------------#
plot(1:10, 1:10, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type =
"n")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
"white")
text(5.5, 5.5, expression(paste(over(sum(paste("(",italic(x[i] -
bar(x)),")"^2), italic(i)==1, italic(n)),(italic(n) - 1)), " = 78.4")),
cex = 1.5)
axis(side = 1, at = c(2, 4, 6, 8), labels = c("a", "b", "c", "d"))
mtext(side = 1, expression(paste("Variance of ", CO[2], "
concentration")), line = 3)
par(op)



Fig.A.8<-function(){
Downs<- data.frame(Age=c(17,20.5,21.5,29.5,30.5,38.5,39.5,40.5,44.5,45.5,47), 
Births=c(13555,22005,23896,15685,13954,4834,3961,2952,596,327,249),
Cases=c(16, 22,16,9,12,15,30,31,22,11,7))
attach(Downs)
plot(Age,Cases/Births)
}

# Fig A.10

Fig.A.10<-function(){
install.packages(deSolve)
library(deSolve)

xstart <- c(N1 = 10, N2 = 10)
pars <- list(r1 = 0.5, r2 = 0.4, K1 = 400, K2 = 300, a2.1 = 0.4, a1.2 =
1.1)

LV<-function(time=time, xstart=xstart, pars=pars){
N1<-xstart[1]
N2<-xstart[2]
with(as.list(pars),{
dn1<-r1*N1*((K1-N1-(a1.2*N2))/K1)
dn2<-r2*N2*((K2-N2-(a2.1*N1))/K2)
res<-list(c(dn1,dn2))
})}

plot(out$time, out$N2, xlab = "Time", ylab = "Number of individuals",
type = "l")
lines(out$time, out$N1, type = "l", col = "red", lty = 2)
legend("bottomright", lty = c(1, 2), legend = c("Species 2", "Species
1"), col = c(1, 2))
}

###############################################
#radiation.heatl calculates annual incident solar radiation (MJ cm-2 yr-1) and heatload 
#(a radiation index based on the idea that highest amounts of radiation occur on southwest facing slopes in the northern hemisphere).  
#The function requires three types of data: slope (measured in degrees), aspect, and latitude.  The function is based on 
#equations from a paper written by McCune and Keon (2002).  Note that this function ignores climatic factors including 
#cloudiness, and is therefore is probably best for relative comparisons of radiation and heatload within a region and 
#not for absolute measurements. 
#McCune, B., and D. Keon. 2002:  Equations for potential annual direct radiation and heat 
#load.  Journal of Vegetation Science, 13: 603-606.


radiation.heatl<-function(slope,aspect,lat){
asp.wrap.rad<-sapply(aspect,function(x){(-1*abs(x-180))+180})
asp.wrap.hl<-sapply(aspect,function(x){abs(180-abs(x-225))})
rad.lat<-(lat/180)*pi
rad.asp<-sapply(asp.wrap.rad,function(x){(x/180)*pi})
hl.asp<-sapply(asp.wrap.hl,function(x){(x/180)*pi})
rad.slope<-sapply(slope,function(x){(x/180)*pi})
rad<-matrix(nrow=length(slope),ncol=1)
hl<-matrix(nrow=length(slope),ncol=1)
for(i in 1:length(slope)){
rad[i]<-0.339+0.808*(cos(rad.lat)*cos(rad.slope[i]))-0.196*(sin(rad.lat)*sin(rad.slope[i]))-0.482*(cos(rad.asp[i])*sin(rad.slope[i]))
hl[i]<-0.339+0.808*(cos(rad.lat)*cos(rad.slope[i]))-0.196*(sin(rad.lat)*sin(rad.slope[i]))-0.482*(cos(hl.asp[i])*sin(rad.slope[i]))
}
result<-list()
result$radiation<-rad
result$heat.load<-hl
result
}

###############################################
#Gamma diversity. species richness over a range of habitats (Whittaker 1972).  The simplest and probably most informative measure of
#gamma diversity is simply the total richness across a landscape, i.e. the total number of species encountered at all sampled plots.  

W.Gamma.div<-function(comm.matrix){	
dm<-apply(comm.matrix>0,2,sum)
gamma<-sum(sapply(dm>0,sum))
gamma
}


###############################################
#Simpson’s index (D) has a straightforward interpretation.  
#It is the probability of reaching into a plot and simultaneously pulling out two different species.  
#The function has been attributed to Simpson (1949)
#Simpson, E. H.  1949.  Measurement of diversity.  Nature.  163: 688.   

Simp.index<-function(comm.matrix){
p.i<-apply(comm.matrix,1,function(x){x/sum(x)})
D<-1-apply(p.i^2,2,sum)
D
}

###############################################
#The Shannon Weiner index (MacArthur and MacArthur 1961) is an information based criterion for diversity.
#MacArthur, R. H., and MacArthur J. W.  1961.  On bird species diversity.  Ecology.  42: 594-598.

SW.index<-function(comm.matrix){
p.i<-apply(comm.matrix,1,function(x){x/sum(x)})
h<-apply(p.i,1,function(x){log(x)*x})
h.prime<- -1*apply(h,1,function(x){sum(x[!is.na(x)])})
h.prime
}






###############################################


#Whittaker's beta diversity. 
#Whittaker, R. H.  1960.  Vegetation of the Sisikyou Mountains, Oregon and California. Ecological Monographs.  30: 279-338.


W.beta.div<-function(taxa){
kappa<-mean(apply(taxa>0,1,sum))
gamma<-sum(apply(taxa>0,2,sum)>0)
W.beta<-(gamma/kappa)-1
W.beta
}




#
#####################################################

Fig.A.11<-function(){
library(labdsv)
data(brycesite)
attach(brycesite)
hist(slope,xlab="Slope (Degrees)",ylab="Relative Frequency of Observations", freq=FALSE,main="")
}

######################################################

Fig.A.12<-function(){
hist(av,ylab="Relative Frequency of Observations",xlab="Aspect Value",main="",freq=FALSE) 
}

#######################################################

Fig.A.13<-function(){
plot(av,annrad,xlab="Aspect Value",ylab="Annual Radiation (Langleys)")
}

#######################################################

Fig.A.14<-function(){
plot(slope~pos,ylab="Slope (degrees)",xlab="Topographic Position")
}

#######################################################

#Fig.A.11<-function(){
#plot(av[slope>10],annrad[slope>10],xlab="Aspect value",ylab="Annual Radiation (Langleys)")
#}

#######################################################

Fig.A.15<-function(){
library(vegan)
data(varespec)
data(varechem)
attach(varespec)
attach(varechem)
plot(N,pH,xlab="% soil N",pch=16,cex=Vac.vit/100*15)}  

#######################################################
attach(varechem); attach(varespec) 
Fig.A.16<-function(angle = 55){
require(scatterplot3d)
s3d <- scatterplot3d(cbind(N, pH, Vac.vit), type="h", highlight.3d = TRUE,
angle = angle, scale = .7, pch = 16, xlab = "N", ylab = "pH", zlab = expression(paste(italic(Vaccinium), " ", italic(vitis-idaea), " % cover"))) 
lm1 <-lm(Vac.vit ~ N + pH) 
s3d$plane3d(lm1)
}
Fig.A.16() 
 
rotate.Fig.A.16(){
 angle = seq(0:360)
 for(i in 1:length(angle)){
 dev.hold()
 Fig.A.16(angle = angle[i])
 dev.flush()
 Sys.sleep(.1)
}
} 
#######################################################


library(asbio)
data(world.co2)
data(world.pop)

#We will give the datasets shorter names.
co2<-world.co2
wp<-world.pop

#The CO2 data has two additional countries (columns) compared to the world population data: Belgium and Ghana.  We will get rid of these columns and the "year" #column in the CO2 dataset.
co2.1<-co2[,-c(1,3,8)]

#We will also want to look at the 2006 CO2 data by itself.  It is in row 27.  
co2.2006<-co2.1[27,]

#The names of some of the counties are too long to fit on the X-axis for the barplot we wish to create (Fig. A.14). We can deal with this in several ways. 1) We can #decrease the font-size of the names using the cex.names argument (the default for barplot names is cex.names =1).  2) We can make the country names perpendicular to#the X-axis instead of parallel using the las argument. 3) We can simply make the country names shorter.  We can do this by changing the column names of the#dataframe or matrix.  For instance we could use some variant on:

colnames(co2.1)<-c("Afgan.","Brazil","Canada","China","Finland","Italy","Japan","Kenya","Mexico","S. Arabia","UAR","US","Total")
colnames(co2.2006)<-colnames(co2.1)

Fig.A.17<-function(){
barplot(as.matrix(co2.2006),las=3,ylab=expression(paste("2006 ", CO[2]," Emissions (metric tons x ", 10^6,")")))
}

#######################################################

Fig.A.18<-function(){
par(mar = c(6,4.6,2,1))
barplot(as.matrix(co2.1[,1:12]),las=3,ylab=expression(paste(CO[2]," Emissions (metric tons x ", 10^6,")")),col=gray(seq(0:26)/27))
legend("topleft",fill=gray(seq(0,26,1)/27),legend=seq(1980,2006,1),cex=.5)
}


#######################################################

Fig.A.19<-function(){
plot(seq(1980,2006,1),co2.1[,1],ylab=expression(paste(CO[2]," Emissions (metric tons x ", 10^6,")")),type="l",xlab="Year",ylim=c(min(co2),max(co2)))
lines(seq(1980,2006,1),co2.1[,2],type="l",col=gray(1/13),lty=2)
lines(seq(1980,2006,1),co2.1[,3],type="l",col=gray(2/13),lty=3)
lines(seq(1980,2006,1),co2.1[,4],type="l",col=gray(3/13),lty=4)
lines(seq(1980,2006,1),co2.1[,5],type="l",col=gray(4/13),lty=5)
lines(seq(1980,2006,1),co2.1[,6],type="l",col=gray(5/13),lty=6)
lines(seq(1980,2006,1),co2.1[,7],type="l",col=gray(6/13),lty=1)
lines(seq(1980,2006,1),co2.1[,8],type="l",col=gray(7/13),lty=2)
lines(seq(1980,2006,1),co2.1[,9],type="l",col=gray(8/13),lty=3)
lines(seq(1980,2006,1),co2.1[,10],type="l",col=gray(9/13),lty=4)
lines(seq(1980,2006,1),co2.1[,11],type="l",col=gray(10/13),lty=5)
lines(seq(1980,2006,1),co2.1[,12],type="l",col=gray(11/13),lty=6)
lines(seq(1980,2006,1),co2.1[,13],type="l",col=1,lty=1,lwd=2)
legend("topleft",col=c(gray(seq(0,11,1)/13),1),lty=c(seq(1:6),seq(1:6),1),lwd=c(rep(1,12),2),legend=colnames(co2.1[,1:13]),cex=.7, bg = "white")
}

##########################################################

Fig.A.20<-function(){
wp.1<-wp[,-c(1)]
per.cap<-(10^6)*co2.1/wp.1
plot(seq(1980,2006,1),per.cap[,1],ylab=expression(paste(CO[2]," Emissions (metric tons)")),type="l",xlab="Year",ylim=c(min(per.cap[,1:12]),max(per.cap[,1:12])))
lines(seq(1980,2006,1),per.cap[,2],type="l",col=gray(1/11),lty=2)
lines(seq(1980,2006,1),per.cap[,3],type="l",col=gray(2/11),lty=3)
lines(seq(1980,2006,1),per.cap[,4],type="l",col=gray(3/11),lty=4)
lines(seq(1980,2006,1),per.cap[,5],type="l",col=gray(4/11),lty=5)
lines(seq(1980,2006,1),per.cap[,6],type="l",col=gray(5/11),lty=6)
lines(seq(1980,2006,1),per.cap[,7],type="l",col=gray(6/11),lty=1)
lines(seq(1980,2006,1),per.cap[,8],type="l",col=gray(7/11),lty=2)
lines(seq(1980,2006,1),per.cap[,9],type="l",col=gray(8/11),lty=3)
lines(seq(1980,2006,1),per.cap[,10],type="l",col=gray(9/11),lty=4)
lines(seq(1980,2006,1),per.cap[,11],type="l",col=gray(10/11),lty=5)
lines(seq(1980,2006,1),per.cap[,12],type="l",col=gray(0),lty=6,lwd=2)
legend("topleft",col=c(gray(seq(0,10,1)/11),1),lty=c(seq(1:6),seq(1:6),2),lwd=c(rep(1,12),2),legend=colnames(co2.1[1:12]),cex=.7,bg="white")
} 
