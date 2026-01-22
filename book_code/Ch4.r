# Fig 4.2

Fig.4.2 <- fuction(){
library(boot)
data(melanoma)

Fig.4_2b<-function(){
Male.live<-length(melanoma$time[melanoma$sex==1&melanoma$status==1])
Male.die<-length(melanoma$time[melanoma$sex==1&melanoma$status==2])
Female.die<-length(melanoma$time[melanoma$sex==0&melanoma$status==2])
Female.live<-length(melanoma$time[melanoma$sex==0&melanoma$status==1])
Fem<-c(Female.die,Female.live)
Male<-c(Male.die,Male.live)
mel<-rbind(Male,Fem)
colnames(mel)<-c("Dead","Alive")
barplot(mel,legend.text=c("Male","Female"),ylab="Count",xlab="Patients with malignant melanoma",cex.lab = 1.2, cex.axis=1.1)
} 

library(asbio)
data(bombus)
data(Glucose2)
attach(Glucose2)

Fig.4_2d<-function(){
plot(Time,glucose,xlab="Time after alchohol ingestion (min/10)",ylab="Blood glucose conc. (ml/dl)",type="n", cex.axis = 1.2, cex.lab = 1.2)
lines(Time[Subject==1&Date==1],glucose[Subject==1&Date==1],col=gray(0),lty=1, lwd = 2)
lines(Time[Subject==2&Date==1],glucose[Subject==2&Date==1],col=gray(0),lty=2, lwd = 2)
lines(Time[Subject==3&Date==1],glucose[Subject==3&Date==1],col=gray(0),lty=3, lwd = 2) 
lines(Time[Subject==4&Date==1],glucose[Subject==4&Date==1],col=gray(0),lty=4,lwd = 2)
lines(Time[Subject==5&Date==1],glucose[Subject==5&Date==1],col=gray(0.5),lty=1, lwd = 2)
lines(Time[Subject==6&Date==1],glucose[Subject==6&Date==1],col=gray(0.5),lty=2, lwd = 2)
lines(Time[Subject==7&Date==1],glucose[Subject==7&Date==1],col=gray(0.5),lty=3, lwd = 2)
legend("topright",legend=seq(1,7),col=c(rep(1,4),rep("gray",3)),lty=c(1,2,3,4,1,2,3),title="Subject",xpd=.5, lwd = 2)
}



par(mfrow=c(2,2),mar=c(5,4.1,1,1))
#4.2a
stem(bombus[,1])
plot(0:10, type="n",xaxt="n", yaxt="n", ylab = "", xlab = "")
mtext(side = 3, "(a)", cex = 1.7, at = -1.4, line = -.8) 
mtext(side = 1, "Pollen collection proportions", line = 2, cex=1) 
text(4,5, "  0.0 | 7
  0.1 | 01259
  0.2 | 88
  0.3 | 0145789
  0.4 | 001227889
  0.5 | 0112389
  0.6 | 00559
  0.7 | 0000446789
  0.8 | 9", 
  adj = 0, cex = 1
  )
#4.2b
Fig.4_2b()
mtext(side = 3, "(b)", cex = 1.7, at = -.43, line = -.7) 
#4.2c
plot(Time,glucose,xlab="Time after alchohol ingestion (min/10)",ylab="Blood glucose conc. (ml/dl)", cex.axis = 1.2, cex.lab = 1.2)
mtext(side = 3, "(c)", cex = 1.7, at = -8.6, line = 0) 
#4.2d
Fig.4_2d()
mtext(side = 3, "(d)", cex = 1.7, at = -9.8, line = 0) 
}
 
#boxplot(melanoma[,6]~melanoma[,3], ylab="Tumor thickness (mm)",names = c("Male", "Female"))
#hist(bombus[,1], main = "", xlab = "Proportion of pollen removed")

Fig.4.3<-function(){
pop<-c(rnorm(300,0,5),runif(100,0,100))
truehist(pop,breaks=seq(-20,100,2),col="white",prob=F, ylab="Frequency",xlab= expression(italic(x)), cex.lab = 1.3, cex.axis = 1.3, border= gray(.4))
segments(median(pop),0,median(pop),60,lty=1,lwd=3)
segments(mean(pop),0,mean(pop),60,lty=2,lwd=3)
points(mean(pop),-10,pch=17,cex=15)
legend("topright",lty=c(1,2),legend=c("Median","Mean"), cex= 1.3, lwd = 3, bty = "n", inset = .1)

###########################################
# Fig.4.5

trag <- c(59, 49, 75, 43, 94, 68, 77, 78, 63, 71, 31, 59, 53, 48, 65, 73, 50, 59, 50, 57)
anm.ls(trag, parameter = "mu", est.lty = 2, est.col = gray(.4),
xlab=expression(paste("Estimates for ", italic(E),"(",italic(X),")", sep = "")))

###########################################
# Fig.4.6
anm.loglik(trag, dist="norm", parameter="mu", est.col = gray(.4))

###########################################
# Fig.4.7
set.seed(1)
x <- rexp(20)
dev.new(height=3.5,width= 7)
par(mfrow = c(1,2), mar=c(2,2,0,0.5), oma = c(2,2,0.5,0))

anm.loglik(x, dist="norm", parameter="mu", est.col = gray(.4), anim = F, plot.likfunc =F, density.leg = F, cex.leg = .8)
anm.loglik(x, dist="exp", est.col = gray(.4), anim = F, plot.likfunc =F, density.leg = F, cex.leg = 0.8)
mtext(side = 1, expression(italic(x)), outer = T)
mtext(side = 2, expression(paste(italic(f),"(",italic(x),")", sep = "")), outer = T, line = 0.75)

############################################
# Fig.4.8
trag <- c(59, 49, 75, 43, 94, 68, 77, 78, 63, 71, 31, 59, 53, 48, 65, 73, 50, 59, 50, 57)
trag.sub <- sample(trag, 10, replace = F); p <- seq(-20, 150, .1)
dev.new(height=3.5,width= 7)
par(mfrow = c(1,2), mar=c(2,2,0,0.5), oma = c(2,2,2,0))
anm.loglik(trag, parameter = "mu", dist = "norm", poss = p,
ylim = c(-200, -80), xlim = c(-20, 150), est.col = gray(.4), density.leg = F, plot.density = F, anim = F)
anm.loglik(trag.sub, parameter = "mu", dist = "norm", poss = p,
ylim = c(-200, -30), xlim = c(-20, 150), est.col = gray(.4), density.leg = F, plot.density = F, anim = F)
mtext(side = 1, expression(paste("Estimates for ", mu)), outer = T, line =1)
mtext(side = 2, "Normal log-likelihood function", outer = T, line = 1)
mtext(side = 3, "(a)", outer = T, at = -0.02, cex = 1.4, line = .2)
mtext(side = 3, "(b)", outer = T, at = .51, cex = 1.4, line = .2)


#############################################
Fig.4.9<-function(){
thetas<-seq(0,1,.001)#Here is a sequence of thetas
posterior.dist1<-thetas^3*(1-thetas)^(5-3)
posterior.dist2<-thetas^26*(1-thetas)^(50-26)
posterior.dist3<-thetas^51*(1-thetas)^(100-51)
posterior.dist4<-thetas^512*(1-thetas)^(1000-512)
par(mfrow=c(2,2),mar=c(1,2,0.5,1), oma=c(4,3,0.5,1))

pdfFonts(stdsym=Type1Font("standardsymbol",
            c("Helvetica.afm",
              "s050000l.afm",
              "Helvetica-Oblique.afm",
              "Helvetica-Bold.afm",
              "Symbol.afm"),
            encoding="CP1253"))
cx=1.1
cx1= 1
plot(thetas,posterior.dist1,xlim=c(0,1),ylab="",type="l", xlab = "", xaxt = "n",cex.axis=cx1,cex.lab=cx)
legend("topright",expression(paste(italic(x),"=3, ",italic(n),"=5")),bty="n",cex=cx)
axis(1,at=c(0.0,0.2,0.4,0.6,0.8,1.0), labels = FALSE)	
plot(thetas,posterior.dist2,xlim=c(0,1),ylab="",type="l", xlab = "", xaxt="n",cex.axis=cx1,cex.lab=cx)	
legend("topright",expression(paste(italic(x),"=26, ",italic(n),"=50")),bty="n", cex=cx)
axis(1,at=c(0.0,0.2,0.4,0.6,0.8,1.0), labels = FALSE)	
plot(thetas,posterior.dist3,xlim=c(0,1),ylab="",type="l", xlab = "",cex.axis=cx1,cex.lab=cx) 
mtext(side=2,"Posterior density", cex=cx, outer = TRUE, line = 1.5)
legend("topright",expression(paste(italic(x),"=51, ",italic(n),"=100")),bty="n", cex=cx)
plot(thetas,posterior.dist4,xlim=c(0,1),ylab="",xlab = "", type="l", cex.axis=cx1,cex.lab=cx)
mtext(side=1,"\u03B8", font = 3, cex=cx, family = "stdsym", outer = TRUE, line = 2)
legend("topright",expression(paste(italic(x),"=512, ",italic(n),"=1000")), bty="n", cex=cx)

}
