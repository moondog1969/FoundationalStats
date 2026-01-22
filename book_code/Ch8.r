Fig.8.1<-function(){
data(C.isotope)

pdfFonts(stdsym=Type1Font("standardsymbol",
            c("Helvetica.afm",
              "s050000l.afm",
              "Helvetica-Oblique.afm",
              "Helvetica-Bold.afm",
              "Symbol.afm"),
            encoding="CP1253"))

par(mar=c(5,4.5,1,4.5),cex=1.1)
plot(C.isotope[,2],C.isotope[,3], xaxt = "n", xlab = "Year", ylab = expression(paste(CO[2], " concentration (ppm)")))
axis(1, at=round(seq(from=1993, to=2007,length.out=7),1), labels=c("1993","","","2000","","","2007"))
par(new=TRUE)
plot(C.isotope[,2],C.isotope[,4],pch=19, xlab="",ylab="",bty="c",axes=F)
axis(4)
mtext(side=4,expression(paste(delta^14, "C (", "\u2030",")")),line = 3, family = "stdsym", cex=1.1)
legend("top",pch=c(1,19),legend=c(expression(CO[2]), expression(paste(delta^14,"C"))),bty="n")
}


Fig.8.2<-function(){
cx = 1.3
cx1=1.3
cx2=1
dev.new(width=8)
layout(matrix(c(1,1,3,3,3,3,2,2,3,3,3,3), 2, 6, byrow = TRUE))
par(mar=c(4.5,4.7,0.5,0.5), bg = "white")

plot(c(-4,-3,-2,-1,0,1,2,3,4), c(0,0,0,0.25, 0.5, 0.25,0,0,0), pch=19, xlab = expression(italic(y)[1]), ylab = expression(paste(italic(f),"(",italic(y)[1],")")), xlim = c(-3.5, 3.5), ylim=c(0,.5),cex=cx, cex.axis=1.1, cex.lab=cx)
segments(c(-1,0,1), c(0,0,0), c(-1,0,1), c(0.25, 0.5, 0.25))
legend("topright", legend=c(expression(paste(italic(E),"(",italic(Y)[1],") = 0")), expression(paste(italic(Var),"(",italic(Y)[1],") = 0.5"))),bty="n", cex=1.1)

plot(c(-4,-3,-2,-1,0,1,2,3,4), c(0,0,0,0.25, 0.5, 0.25,0,0,0), pch=19, xlab = expression(italic(y)[2]), ylab = expression(paste(italic(f),"(",italic(y)[2],")")), xlim = c(-3.5, 3.5), ylim=c(0,.5),cex=cx, cex.axis=1.1, cex.lab=cx)
segments(c(-1,0,1), c(0,0,0), c(-1,0,1), c(0.25, 0.5, 0.25))
legend("topright", legend=c(expression(paste(italic(E),"(",italic(Y)[2],") = 0")), expression(paste(italic(Var),"(",italic(Y)[2],") = 0.5"))),bty="n", cex=1.1)

xy <- expand.grid(c(-3,-2,-1,0,1,2,3),c(-3,-2,-1,0,1,2,3))
z <- ifelse(xy[,1]==0&xy[,2]==1|xy[,1]==0&xy[,2]==-1|xy[,1]==1&xy[,2]==0|xy[,1]==-1&xy[,2]==0,.25,0)
scatterplot3d(xy[,1],xy[,2],z, xlab = expression(italic(y)[1]), ylab = expression(italic(y)[2]), zlab = expression(paste(italic(f),"(",italic(y)[1],",",italic(y)[2],")")),pch=19,cex=1.3,cex.lab=.9,cex.axis=.8,box=FALSE,type="h")
legend("topright", legend=c(expression(paste(italic(E),"(",italic(Y)[1],italic(Y)[2],") = 0")), expression(paste(italic(Cov),"(",italic(Y)[1],",",italic(Y)[2],") = 0"))),bty="n", cex=1.4)
#"","However,","",expression(paste(italic(f),"(",italic(y)[1],",",italic(y)[2],")",""!="",italic(f),"(",italic(y)[1],")",italic(f),"(",italic(y)[2],")"))
}

##Fig.8.3

bvn.plot(mu = c(0, 0), vr = c(1, 1), cv = 0, res = 0.1,  xlab = expression(italic(y)[1]), ylab = expression(italic(y)[2]), 
zlab = expression(paste(italic(f),"(", italic(y)[1], ",", italic(y)[2], ")")))


bvn.plot(mu = c(3, 2), vr = c(25, 5), cv = -0.2, res = 0.5,xlab = expression(italic(y)[1]), ylab = expression(italic(y)[2]), 
zlab = expression(paste(italic(f),"(", italic(y)[1], ",", italic(y)[2], ")")))


Fig.8.3<-function(){
library(mvtnorm)     
dev.new(height=3.5)
par(mfrow=c(1,4),mar=c (0,0,2,3), oma = c(5, 4.2, 0, 0))
p1 <- rmvnorm(20, mean=c(0, 0),sigma=matrix(2,2,data=c(1,1, 1,1)))
plot(p1,xlab="",ylab=expression(Y[2]),main=bquote(paste(italic(r)," = ",.(round(cor(p1)[2],2)))))

p1 <- rmvnorm(20, mean=c(0, 0),sigma=matrix(2,2,data=c(2,-1,-1,2)))
plot(p1,xlab="",ylab="",main=bquote(paste(italic(r)," = ",.(round(cor(p1)[2],2)))))

p1 <- rmvnorm(20, mean=c(0, 0),sigma=matrix(2,2,data=c(1, 1, 1,1)))
p1<-rbind(p1,matrix(nrow=1,ncol=2,data=c(-2,4)))
plot(p1,xlab="",ylab="",main=bquote(paste(italic(r)," = ",.(round(cor(p1)[2],2)))))

p1<-seq(-9,10)
p2<-p1^2-p1
plot(p1,p2, xlab="",ylab="",main=bquote(paste(italic(r)," = ",.(round(cor(p1,p2),2)))))
mtext(expression(italic(y[1])),side=1, outer = T, line = 3)
mtext(expression(italic(y[2])),side=2, outer = T, line = 2.6)
mtext("(a)",side=3, at = -94, line = .5)
mtext("(b)",side=3, at = -67, line = .5)
mtext("(c)",side=3, at = -40, line = .5)
mtext("(d)",side=3, at = -13, line = .5)
}


# Fig. 8.5
dev.new(height=3.5)
par(mfrow=c(1,4),mar=c (0,0,2,3), oma = c(5, 4.2, 0, 0))
with(anscombe, plot(x1, y1, xlab = "", ylab = "", main = bquote(paste(italic(r)," = ",.(round(cor(x1, y1),2)))))); abline(3,0.5) 
with(anscombe, plot(x2, y2, xlab = "", ylab = "",, main = bquote(paste(italic(r)," = ",.(round(cor(x2, y2),2)))))); abline(3,0.5) 
with(anscombe, plot(x3, y3, xlab = "", ylab = "",, main = bquote(paste(italic(r)," = ",.(round(cor(x3, y3),2)))))); abline(3,0.5) 
with(anscombe, plot(x4, y4, xlab = "", ylab = "",, main = bquote(paste(italic(r)," = ",.(round(cor(x4, y4),2)))))); abline(3,0.5) 
mtext(expression(italic(y[1])),side=1, outer = T, line = 3)
mtext(expression(italic(y[2])),side=2, outer = T, line = 2.6)
mtext("(a)",side=3, at = -42, line = .5)
mtext("(b)",side=3, at = -26, line = .5)
mtext("(c)",side=3, at = -10.3, line = .5)
mtext("(d)",side=3, at = 5.5, line = .5)


# Fig 8.7
par(cex=1.3)
data(crab.weight)
plot(crab.weight, xlab="Gill weight (mg)", ylab = "Body weight (g)")

# Fig 8.8
par(cex=1.3)
data(bats)
plot(bats, xlab = "Age (days)", ylab = "Forearm length (mm)")