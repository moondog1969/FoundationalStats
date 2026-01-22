
#negative binomial pdf
nb<-function(x,r,p,mu=NULL){
if(!is.null(mu)) p<-r/(r+mu)
q<-1-p
f.x<-gamma(x+r)/(gamma(r)*factorial(x))*p^r*q^x
f.x
}

Fig.3.1 <- function(){
par(mar = c(5, 4, 2, 4), cex = 1.3)
hist(c(rep(1, 2), rep(2, 5), rep(3, 3)), breaks=c(0, 1, 2, 3), main = "", xlab = expression(italic(x)))
axis(4, at = 0:5, labels = seq(0, 0.5, 0.1))
mtext(expression(paste("Density = ",italic(f),"(",italic(x),")")), 4, line = 2.7, cex = 1.3) 
}

############# Conceptualization of a continuous pdf 

Fig.3.2 <- function(){
windows(height=3.5,width=7); par(mfrow=c(1,4),mar=c(0,0,0,0),oma=c(5,5,0,0));xl<-c(-5,5); xs="r"; ys="r"; p<-rnorm(100000)
yl<-c(0,.5)
hist(p,breaks=seq(-5,5,by=1),freq=F,ylim=yl, xlim=xl,main = "",  xlab="",xaxs=xs,yaxs=ys)
hist(p,breaks=seq(-5,5,by=.5),freq=F,ylim=yl, xlim=xl, main = "",xlab="", xaxs=xs,yaxs=ys,yaxt="n")
hist(p,breaks=seq(-5,5,by=.1),freq=F,ylim=yl, xlim=xl,main = "", xlab="" ,xaxs=xs,yaxs=ys,yaxt="n")
curve(dnorm(x),-5,5,ylim=c(0,.5), xlab="",yaxt="n",bty="n")
mtext(expression(paste("Density = ", italic(f),"(",italic(x),")")),side=2,outer=T,line=3)
mtext(expression(italic(x)),side=1,outer=T,line=3)
}

##############################################################


Fig.3.3 <- function(){
dev.new(height = 3.5)
par(mfrow = c(1, 2), mar = c(4.4, 4.2, 0.5, 0.3))

##pdf
x <- c(0, 1, 2, 3, 4)
f.x <- c(0.5, 0.3, 0.1, 0.05, 0.05)
plot(x, f.x, xlab = expression(italic(x)), ylab = expression(paste(italic(f),"(",italic(x),")")), pch = 16)
mtext(side = 3,at = -1.25, "(a)", line = -1, cex = 1.4)
segments(x, 0, x, f.x)

##cdf
X <- c(0, 1,2,3,4)
F.x <- c(0.5, 0.8, 0.9, 0.95, 1.0)
plot(x, F.x, xlab =  expression(italic(x)), ylab = expression(paste(italic(F),"(",italic(x),")")), pch = 16)
segments(x, F.x, x+1, F.x)
mtext(side = 3, at = -1.25, "(b)", line = -1, cex = 1.4)
points(c(1, 2, 3, 4), c(0.5, 0.8, 0.9, 0.95))
}


### Fig 3.5 ######

par(mar=c(5,4.4,2,2))
shade.bin(x=25, p = .68, n=50, cex.lab=1.15, cex.axis=1.1, legend.cex=1, digits=4, xlim=c(10,50))

### Fig 3.6 ######

par(mar=c(5,4.4,2,2))
shade.bin(x=25, p = .68, n=50, tail = "lower", cex.lab=1.1, cex.axis=1.1, legend.cex=1, digits=4, xlim=c(0,45), ylim=c(0,.14))



Fig.3.9<-function(){ # with plotmath
par(mar=c(5,4.3,2,2))
cx = 1.3; ln = 1
curve(dnorm(x,0,2),from=-6,to=6,xlab=expression(italic(x)),ylab=expression(paste(italic(f),"(",italic(x),")")),xaxt="n", cex.lab=cx, cex.axis=cx)
xl<-c(-6,-4,-2,2,4,6)
yl<-dnorm(xl,0,2)
segments(xl,0,xl,yl)
segments(0,0,0,dnorm(0,0,2),lty=2,col="gray")
mtext(side=1,at=-6.2,line = ln, "-3", cex=cx)
mtext(side=1,at=-5.8,line = ln, expression(sigma), font = 1, cex=cx)
mtext(side=1,at=-4.2,line = ln,"-2", cex=cx)
mtext(side=1,at=-3.8,line = ln,expression(sigma), font = 1, cex=cx)
mtext(side=1,at=-2.2,line = ln,"-1", cex=cx)
mtext(side=1,at=-1.8,line = ln,expression(sigma), font = 1, cex=cx)
mtext(side=1,at=0,line = ln,expression(mu), font = 1, cex=cx)
mtext(side=1,at=1.9,line = ln,"1", cex=cx)
mtext(side=1,at=2.2,line = ln,expression(sigma), font = 1, cex=cx)
mtext(side=1,at=3.9,line = ln,"2", cex=cx)
mtext(side=1,at=4.2,line = ln,expression(sigma), font = 1, cex=cx)
mtext(side=1,at=5.9,line = ln,"3", cex=cx)
mtext(side=1,at=6.2,line = ln,expression(sigma), font = 1, cex=cx)
arrows(-.85,c(0,0.02,0.04),c(-6,-4,-2), c(0,0.02,0.04),length=.1,angle=20,lwd=2)
arrows(.85,c(0,0.02,0.04),c(6,4,2), c(0,0.02,0.04),length=.1,angle=20,lwd=2)
text(c(0,0,0),c(0,0.02,0.04),c(" 99.7%"," 95%"," 68%"), cex=1.2)
axis(1, at=c(-6, -4, -2, 0, 2, 4, 6), labels=c("","","","","","",""))
}

pdfFonts(stdsym=Type1Font("standardsymbol",
            c("Helvetica.afm",
              "s050000l.afm",
              "Helvetica-Oblique.afm",
              "Helvetica-Bold.afm",
              "Symbol.afm"),
            encoding="CP1253"))

Fig3.9 <- function(){ # with new font family
par(mar=c(5,4.4,2,2))
curve(dnorm(x,0,2),from=-6,to=6,xlab=expression(italic(x)),ylab=expression(paste(italic(f),"(",italic(x),")")),xaxt="n", cex.lab=1.2, cex.axis=1.2)
xl<-c(-6,-4,-2,2,4,6)
yl<-dnorm(xl,0,2)
segments(xl,0,xl,yl); cx = 1.2; ln = 1; family = "stdsym"
segments(0,0,0,dnorm(0,0,2),lty=2,col="gray")
mtext(side=1,at=-6.2,line = ln, "-3", cex=cx)
mtext(side=1,at=-5.9,line = ln,"\u03C3", font = 3, cex=cx, family = "stdsym")
mtext(side=1,at=-4.2,line = ln,"-2", cex=cx)
mtext(side=1,at=-3.9,line = ln,"\u03C3", font = 3, cex=cx, family = "stdsym")
mtext(side=1,at=-2.2,line = ln,"-1", cex=cx)
mtext(side=1,at=-1.9,line = ln,"\u03C3", font = 3, cex=cx, family = "stdsym")
mtext(side=1,at=0,line = ln,"\u03BC", font = 3, cex=cx, family = "stdsym")
mtext(side=1,at=1.9,line = ln,"1", cex=cx)
mtext(side=1,at=2.1,line = ln,"\u03C3", font = 3, cex=cx, family = "stdsym")
mtext(side=1,at=3.9,line = ln,"2", cex=cx)
mtext(side=1,at=4.1,line = ln,"\u03C3", font = 3, cex=cx, family = "stdsym")
mtext(side=1,at=5.9,line = ln,"3", cex=cx)
mtext(side=1,at=6.1,line = ln,"\u03C3", font = 3, cex=cx, family = "stdsym")
arrows(-.85,c(0,0.02,0.04),c(-6,-4,-2), c(0,0.02,0.04),length=.1,angle=20,lwd=2)
arrows(.85,c(0,0.02,0.04),c(6,4,2), c(0,0.02,0.04),length=.1,angle=20,lwd=2)
text(c(0,0,0),c(0,0.02,0.04),c(" 99.7%"," 95%"," 68%"))
axis(1, at=c(-6, -4, -2, 0, 2, 4, 6), labels=c("","","","","","",""))
}                                                           















###################################################################



###########################################################################
# Chi-squared vs. Z^2

X<-rnorm(10000)
hist(X^2,col="gray",breaks=20,freq=FALSE,xlab=expression(paste(x^2," and q")),xlim=c(0,10),ylab=expression(paste("f(",x^2,") and f(q)")),main="")
curve(dchisq(x,1),from=0,10,add=TRUE,lwd=2)
points(3,0.4,pch=22,bg="gray",cex=1.5)
segments(2.75,0.3,3.25,0.3,lwd=2)
text(6.25,0.4, expression(paste("Density of ",X^2," when X ~ N(0,1)")))
text(4.5,0.3, expression(paste("Q ~ ",chi^2,"(",nu,")")))



Fig.3.12<-function(){
Larrea<-data.frame(class1=c(5,6,9,11,7,9,7,10,10,11,8,8,9,5,4,10,8,7,3,6,12,8,9,8,6),class2=c(9,7,6,5,0,6,7,6,4,8,6,8,4,3,7,3,9,2,4,10,11,5,5,5,5),class3=c(4,4,4,4,4,4,4,5,4,5,4,4,4,4,4,4,4,4,4,4,4,3,4,4,4))
par(mfrow=c(3,1),mar=c(1,4.5,2.8,0.5), oma = c(4,0,0,0))
attach(Larrea)
c1<-hist(class1,breaks=seq(0,max(class1)),plot=F)
dens1<-matrix(nrow=max(class1),ncol=1)
for(i in 0:max(class1)){
dens1[i]<-dpois(i,7.84)}

plot(c1,freq=F,col=rgb(blue=0.4,red=0.4,green=0.4,alpha=.9),main="",xlab="",xaxt="n",cex.axis=1.3, cex.lab=1.3)
poi<-c1;poi$density<-dens1
plot(poi,freq=F,add=T,col=rgb(red=1,blue=1,green=1,alpha=.3))
legend("topright",legend=c("Sample",expression(paste("POI(",italic(lambda), "=7.84)")), "Overlap"),pch=22,pt.cex=1.7,pt.bg=c(rgb(red=0.4,blue=0.4,green=0.5,alpha=.9), "white",gray(.8)),cex=1.1)
mtext(side=3,at=2,expression(paste("Age Class 1  ",10^2,"-",10^3, "cm")),cex=.85,line=1)

c2<-hist(class2,breaks=seq(0,max(class1)),plot=F)
dens2<-matrix(nrow=max(class2),ncol=1)
for(i in 0:max(class1)){
dens2[i]<-dpois(i,5.8)}

plot(c2,freq=F,col=rgb(blue=0.4,red=0.4,green=0.4,alpha=.9),main="",xlab="", xaxt = "n",cex.axis=1.3,cex.lab=1.3)
poi<-c2;poi$density<-dens2
plot(poi,freq=F,add=T,col=rgb(red=1,blue=1,green=1,alpha=.3))
legend("topright",legend=c("Sample",expression(paste("POI(",italic(lambda), "=5.8)")), "Overlap"),pch=22,pt.cex=1.7,pt.bg=c(rgb(red=0.4,blue=0.4,green=0.5,alpha=.9), "white",gray(.8)),cex=1.1)
mtext(side=3,at=2,expression(paste("Age Class 2  ",10^3,"-",10^4,"cm")),cex=.85,line=1)

c3<-hist(class3,breaks=seq(0,max(class1)),plot=F)
dens3<-matrix(nrow=max(class3),ncol=1)
for(i in 0:max(class1)){
dens3[i]<-dpois(i,4.04)}

plot(c3,freq=F,col=rgb(blue=0.4,red=0.4,green=0.4,alpha=.9),main="",xlab="",cex.axis=1.3,cex.lab=1.3)
poi<-c3;poi$density<-dens3
plot(poi,freq=F,add=T,col=rgb(red=1,blue=1,green=1,alpha=.3))
legend("topright",legend=c("Sample",expression(paste("POI(",italic(lambda), "=4.04)")), "Overlap"),pch=22,pt.cex=1.7,pt.bg=c(rgb(red=0.4,blue=0.4,green=0.5,alpha=.9), "white",gray(.8)), cex = 1.1)
mtext(side=3,at=2,expression(paste("Age Class 3  ",10^4,"-",10^5, "cm")),cex=.85,line=1)
mtext(side=1,outer = TRUE, expression(paste(italic("Larrea")," counts")), line = 2, cex=.9)
}



###########################################################################################0

Fig.3.16<-function(){
par(mfrow=c(3,2),mar=c(4.4,4.5,0.5,0.5))
#a
curve(dnorm(x),from=-3,to=3,xlab=expression(italic(x)),ylab= expression(paste(italic(f),"(",italic(x),")", sep="")),lty=1,ylim=c(0,0.5))
text(1.75,0.47,expression(paste(italic(X)," ~ ",italic(N),"(0,1)", sep = "")))
mtext(side=3,at=-4.35,"(a)",line=-1,cex=.9)
#b
curve(dt(x,1),from=-3,to=3,xlab=expression(italic(x)),ylab= expression(paste(italic(f),"(",italic(x),")", sep="")),lty=1,ylim=c(0,0.5))
curve(dt(x,5),from=-3,to=3,add=TRUE,lty=2)
curve(dt(x,10),from=-3,to=3,add=TRUE,lty=3)
legend("topright",lty=seq(1,3),legend=c(expression(paste(nu,"=1")),expression(paste(nu,"=5")),expression(paste(nu,"=10"))),bty="n",title=expression(paste(italic(X)," ~ ",italic(t),"(",nu,")", sep = "")))
mtext(side=3,at=-4.35,"(b)",line=-1,cex=.9)
#c
curve(df(x,1,1),from=0,to=3,xlab=expression(italic(x)),ylab= expression(paste(italic(f),"(",italic(x),")", sep="")),lty=1,ylim=c(0,0.8))
curve(df(x,5,5),from=0,to=3,add=TRUE,lty=2)
curve(df(x,10,10),from=0,to=3,add=TRUE,lty=3)
legend("topright",lty=seq(1,3),legend=c(expression(paste(nu[1]," = ",nu[2]," = 1")), expression(paste(nu[1]," = ",nu[2]," = 5")), expression(paste(nu[1], " = ",nu[2]," = 10"))),bty="n",title=expression(paste(italic(X)," ~ ",italic(F),"(",nu[1],",",nu[2],")", sep="")))
mtext(side=3,at=-.7,"(c)",line=-1,cex=.9)
#d
curve(dchisq(x,1),from=0,to=15,xlab=expression(italic(x)),ylab= expression(paste(italic(f),"(",italic(x),")", sep="")),lty=1,ylim=c(0,0.4))
curve(dchisq(x,5),from=0,to=15,add=TRUE,lty=2)
curve(dchisq(x,10),from=0,to=15,add=TRUE,lty=3)
legend("topright",lty=seq(1,3),legend=c(expression(paste(nu,"=1")),expression(paste(nu,"=5")),expression(paste(nu,"=10"))),bty="n",title=expression(paste(italic(X)," ~ ",chi^2,"(",nu,")", sep = "")),inset =0.02)
mtext(side=3,at=-3.3,"(d)",line=-1,cex=.9)
#e
curve(dlnorm(x),from=0,to=5,xlab=expression(italic(e^x)),ylab= expression(paste(italic(f),"(",italic(e^x),")")),lty=1,ylim=c(0,0.7))
text(4,0.67,expression(paste(italic(X)," ~ ",italic(N),"(0,1)", sep = "")))
mtext(side=3,at=-1.12,"(e)",line=-1,cex=.9)
#f
curve(dweibull(x,3.25,3.25),from=0,to=6,xlab=expression(italic(x)),ylab= expression(paste(italic(f),"(",italic(x),")", sep="")),lty=1,ylim=c(0,0.5))
text(4.7,0.47,expression(paste(italic(X)," ~ ",italic(WEI),"(3.25,3.25)",sep="")))
mtext(side=3,at=-1.3,"(f)",line=-1,cex=.9)
}

#############################################################################

### Fig 3.17 ######

par(mar=c(5,4.4,2,2))
shade.chi(x=7, nu = 10, tail = "upper", cex.lab=1.15, cex.axis=1.1, legend.cex=1, digits=4, xlim=c(10,50))

### Fig 3.18 ######

par(mar=c(5,4.5,2,2))
shade.t(x = 2, nu = 5, tail = "two", cex.lab=1.35, cex.axis=1.3, legend.cex=1.25, digits=4, xlim=c(-7,7))

### Fig 3.21 ######

par(mar=c(5,4.4,2,2))
shade.F(x = 2, nu1 = 15, nu2 = 12, tail = "middle", from = 1, to = 3, cex.lab=1.15, cex.axis=1.1, legend.cex=1, digits=4, xlim=c(0,7))

Fig.3.22<-function(){
cx <- 1.2
dev.new(height = 4.5, width = 9)
n.x <- c(115, 25, 19, 12, 2, 1, 0)
theta <- sum(n.x/sum(n.x) * seq(0.5, 6.5, 1))# weighted mean for age
theta

age <-seq(0,6,1)
par(mfrow=c(1,2),mar=c(4.4,4.5,0.5,0.5))
plot(age[1:6], n.x[1:6],xlab="Bird age (years)",log="y",ylab="Number of survivors", cex = cx - .1, cex.axis = cx, cex.lab = cx)
abline(lm(log(n.x[1:6],base=10)~age[1:6]))
mtext(side=3,at=-1.3,"(a)", line=-1,cex=1.6)
plot(age, n.x/115,xlab="Bird age (years)",ylab="Survivorship probability", cex = cx - .1, cex.axis = cx, cex.lab = cx)
lines(age,exp(-theta*age))
mtext(side=3,at=-1.6,"(b)", line=-1,cex=1.6)
}

###############################################################################

## Fig. 3.26
data(BCI.count)
BCI.ttl<-apply(BCI.count,2,sum)

Preston.dist(BCI.ttl, cex = 1.3, cex.lab = 1.3, cex.axis = 1.3, cex.octave = 1.2, cex.legend = 1.2, cex.pt = 1.2)


###########################################################################

Fig.3.28<-function(){

library(boot)
data(catsM)
cats.ecdf<-ecdf(catsM[,2])
x<-plot.stepfun(cats.ecdf)$t
y<-plot.stepfun(cats.ecdf)$y

x1 <- c(x[-c(1,2)],10)
y1 <- c(y[-1],1)

par(mar=c(5,4.5,2,2))
plot(cats.ecdf, main= "", xlab = expression(italic(x)), ylab = expression(paste(italic(Fn),"(",italic(x),")", sep = "")), cex = 1.3, cex.axis=1.3, cex.lab = 1.3)
points(x1,y1,cex=1.3)
curve(pnorm(x,mean(catsM[,2]),sd(catsM[,2])),from=1.5,to=4.2,add=T,lty = 2, lwd=2)
legend("topleft",inset=.1,bty="n",legend=expression(paste(italic(X)," ~ ",italic(N),"(2.9, 0.2185)")),lty = 2, lwd=2, cex= 1.2)}

############################################################################