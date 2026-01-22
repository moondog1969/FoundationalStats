Fig.6.1 <- function(){
par(mar = c(5,4.5,1,1), cex = 1.3)
xv<-seq(-3,3,by = 1/1000)
yv<-dnorm(xv)

curve(dnorm(x), -3, 3, xlab = expression(paste(italic(z),"* = Value of test statistic under ", H[0], sep = "")),ylab = expression(paste(italic(f),"(",italic(z),"*)", sep = "")), xlim = c(-3.4,3.4), ylim = c(0, 0.46))

polygon(c(xv[xv<=-0.5],-0.5),c(yv[xv<=-0.5],yv[xv==-3]), density = 20)
polygon(c(0.5,xv[xv>=0.5]),c(yv[xv==3],yv[xv>=0.5]), density = 20)

polygon(c(xv[xv<=-2],-2),c(yv[xv<=-2],yv[xv==-3]),density = 20, angle = 135)
polygon(c(2,xv[xv>=2]),c(yv[xv==3],yv[xv>=2]),density = 20, angle = 135)

arrows(-3, 0.42, 3, y1 = 0.42, code = 3, length = 0.1)
text(0, 0.44, expression(paste("Increasing evidence against ", H[0])), cex = 1) 
}

pdfFonts(stdsym=Type1Font("standardsymbol",
            c("Helvetica.afm",
              "s050000l.afm",
              "Helvetica-Oblique.afm",
              "Helvetica-Bold.afm",
              "Symbol.afm"),
            encoding="CP1253"))

Fig.6.2 <-function(){
par(mar = c(0,0,0,0), oma=c(5,4,1,1), cex = 1.3)
shade.norm(x = -1.64, tail = "lower", xlim = c(-3, -0.5), ylim = c(0, 0.2), show.p = F, show.dist = F)
abline(v = -1.64, lty = 2)
text(-2, 0.027, "\u03B1", cex = 1.4, font = 3, family = "stdsym")
arrows(-2.5, .05, -0.8, .05, code = 3, length = 0.1)
text(-1.98, .06, expression(paste("Reject ", H[0]," ")), cex = 1)
text(-1.3, .06, expression(paste("       Fail to reject ", H[0])), cex = 1)
text(-1.69, .15, "Critical Value", cex = 1, srt = 90)
arrows(-2.5, .09, -0.8, .09, code = 3, length = 0.1)
text(-1.98, .1, "Significant ", cex = 1)
text(-1.3, .1, "       Non-significant", cex = 1)
abline(h = 0)

mtext(side = 1, expression(paste(italic(z),"* = Value of test statistic under ", H[0], sep = "")), cex = 1.4, line = 3)
mtext(side = 2, expression(paste(italic(f),"(",italic(z),"*)")), cex = 1.4, line = 2.7)
}

## Fig 6.3
dev.new(height = 4, width = 8)
par(mfrow = c(1, 3), mar = c(0, 0, 0, 0), oma = c(6.5, 5, 1, 0.5), bg = "white")
shade.norm(x = -1.2, show.dist = F, show.p = F, cex.axis = 1.4, shade.col= colors()[614], ylim = c(0, .43))
text(-3.5, 0.42, "(a)", cex = 2.4) 
legend("top", legend=expression(paste(italic(P),"(",italic(Z)<=-1.2,") = 0.115",)), bty = "n", cex = 1.2)
shade.norm(x = -1.2, tail = "upper", show.dist = F, show.p = F, cex.axis = 1.4, shade.col= "blue", yaxt = "n", ylim = c(0, .43))
text(-3.5,0.42, "(b)", cex = 2.4)
legend("top", legend=expression(paste(italic(P),"(",italic(Z)>=-1.2,") = 0.885",)), bty = "n", cex = 1.2) 
shade.norm(x = -1.2, tail = "two", show.dist = F, show.p = F, yaxt = "n", shade.col= "red", cex.axis = 1.4, ylim = c(0, .43))
text(-3.5,0.42, "(c)", cex = 2.4)
legend("top", legend=expression(paste("2[",italic(P),"(",italic(Z)>="|-1.2|",")] = 0.231",)), bty = "n", cex = 1.2)  
mtext(side = 1, expression(paste(italic(z),"* = Value of test statistic under ", H[0], sep = "")), line = 3.8, cex = 1.1, outer = T)
mtext(side = 2, expression(paste(italic(f),"(",italic(z),"*)")), line = 3, cex = 1.1, outer = T)


## 6.4 Example -- one sample z

shade.norm(x = -2.305, tail = "two", legend.cex = .9) 

par(mar = c(5,4.5,1,1), cex = 1.3)
xv<-seq(-4,4,by = 1/1000)
yv<-dnorm(xv)

curve(dnorm(x), -4, 4, xlab = expression(paste(italic(z),"*", " under ",H[0], sep = "")),ylab = expression(paste(italic(f),"(",italic(z),"*)", sep = "")), xlim = c(-4,4), ylim = c(0, 0.44))

polygon(c(xv[xv<=-2.305],-2.305),c(yv[xv<=-2.305],yv[xv==-4]), col = "gray")
polygon(c(2.305,xv[xv>=2.305]),c(yv[xv==4],yv[xv>=2.305]), col = "gray")
legend("top", legend=expression(paste(italic(Z)," ~ ",italic(N),"(0, 1)",",     ","2[",italic(P),"(",italic(Z)>= 2.305,")] = 0.021",)), bty = "n") 
abline(h=0)

## 6.5 Example -- one sample t
par(mar = c(5,4.5,1,1), cex = 1.3)
xv<-seq(-4,4,by = 1/1000)
yv<-dt(xv, 84)

curve(dt(x, 84), -4, 4, xlab = expression(paste(italic(t),"*", " under ",H[0], sep = "")),ylab = expression(paste(italic(f),"(",italic(t),"*)", sep = "")), xlim = c(-4,4), ylim = c(0, 0.44))

polygon(c(xv[xv<=-2.305],-2.305),c(yv[xv<=-2.305],yv[xv==-4]), col = "gray")
polygon(c(2.305,xv[xv>=2.305]),c(yv[xv==4],yv[xv>=2.305]), col = "gray")
legend("top", legend=expression(paste(italic(T)," ~ ",italic(t),"(84)",",     ","2[",italic(P),"(",italic(T)<=-2.305,")] = 0.024",)), bty = "n") 
abline(h=0)

############################################################################


###########################################################################

Fig.6.7<-function(){

Power.graph.lower<-function(x,sigma,n,to,from,mu,title=FALSE,xtext=FALSE,ytext=FALSE,xlm=NULL,...){
norm.pdf <-function(x){1/((sigma/sqrt(n))*sqrt(2*pi))*exp(-1/2*((x-mu)/(sigma/sqrt(n)))^2)}
xv<-seq(from,to,.001)
yv<-norm.pdf(xv)
plot(xv,yv,type="l", cex.main=.9,mar= c(.1,.1,.1,.1),xaxt=ifelse(xtext==TRUE,"s","n"),yaxt=ifelse(ytext==TRUE,"s","n"),xlim=xlm,...)
abline(v=x,col="gray",lty=2)
bX<-bquote(paste(bar(italic(X))," ~ ",italic(N),"(",.(mu),",",.(sigma),"/",.(n),")"))
legend ("topright", legend=bX,bty="n",inset=.01,cex=1.1)
polygon(c(xv[xv<=x],x),c(yv[xv<=x],yv[xv==from]),col="gray")
}

##a##

layout(matrix(c(0,1,0,5,0,2,0,6,0,0,0,0,0,3,0,7,0,4,0,8,0,0,0,0), 6, 4, byrow = TRUE),widths=c(.17,1.33,.07,1.33),heights=c(1.11,1.11,0.12,1.11,1.11,0.44))
par(mar=c(0,0,0,0), oma=c(2,2,0.5,0.5))

cex1 <- 1.3 #oma axes
cex2 <- 1.2 # graph axes
cex3 <- 1.1 #Pvals
cex4 <- 1.1 # legend

Power.graph.lower (x=-5.234,sigma=45, n=200,to=15,from=-16,mu=0,xlm=c(-16,15))
axis(side=2,at=c(0,.1),cex.axis=cex2)
arrows(-10,0.08,-7,0.005,length=.1,angle=20)
text(-9.7,0.087,expression(paste(alpha," = ",italic(P),"(",bar(italic(X))<= -5.23,") = 0.05")),cex=cex3)
mtext("(a)",3,at=-15.5,line=-1.6,cex=cex1)

Power.graph.lower (x=-5.234,sigma=45, n=200,to=15,from=-16,mu=-7,xlm=c(-16,15), cex.axis = cex2)
axis(side=2,at=c(0,.1),cex.axis=cex2)
arrows(5,0.07,-7,0.02,length=.1,angle=20)
pval<-round(pnorm(-5.234,-7,45/sqrt(200)),2)
text(5,0.077,bquote(paste("1-",beta," = ",italic(P),"(",bar(italic(X))<=-5.23,") = ",.(pval))),cex=cex3)


##c##

Power.graph.lower (x=-4.2738,sigma=45, n=300,to=15,from=-16,mu=0,xlm=c(-16,15))
axis(side=2,at=c(0,.1),cex.axis=cex2)
pval<-round(pnorm(-4.2738,0,45/sqrt(300)),2)
arrows(-10,0.08,-7,0.005,length=.1,angle=20)
text(-9.7,0.087,bquote(paste(alpha," = ",italic(P),"(",bar(italic(X))<=-4.27,") = ",.(pval))),cex=cex3)
mtext("(c)",3,at=-15.5,line=-1.6,cex=cex1)
mtext(expression(paste(italic(f),"(",bar(italic(x)),")")),3,at=-23,cex=1.2,las=2)

Power.graph.lower (x=-4.2738,sigma=45, n=300,to=15,from=-16,mu=-7,xlm=c(-16,15),xtext=TRUE, cex.axis = cex2)
axis(side=2,at=c(0,.1),cex.axis=cex2)
arrows(5,0.07,-7,0.02,length=.1,angle=20)
pval<-round(pnorm(-4.2738,-7,45/sqrt(300)),2)
text(5,0.077,bquote(paste("1-",beta," = ",italic(P),"(",bar(italic(X))<=-4.27,") = ",.(pval))),cex=cex3)


##b##

Power.graph.lower (x=-5.234,sigma=45, n=200,to=15,from=-16,mu=0,xlm=c(-16,15))
arrows(-10,0.08,-7,0.005,length=.1,angle=20)
text(-9.7,0.087,expression(paste(alpha," = ",italic(P),"(",bar(italic(X))<=-5.23,") = 0.05")),cex=cex3)
mtext("(b)",3,at=-15.5,line=-1.6,cex=cex1)

Power.graph.lower (x=-5.234,sigma=45, n=200,to=15,from=-16,mu=-8,xlm=c(-16,15))
arrows(4,0.065,-11,0.02,length=.1,angle=20)
pval<-round(pnorm(-5.234,-8,45/sqrt(200)),2)
text(4,0.075,bquote(paste("1-",beta," = ",italic(P),"(",bar(italic(X))<= -5.23,") = ",.(pval))),cex=cex3)

##d##

Power.graph.lower (x= -2.677955,sigma=45, n=200,to=15,from=-16,mu=0,xlm=c(-16,15))
pval<-round(pnorm(-2.677955,0,45/sqrt(200)),2)
arrows(-8,0.07,-5,0.005,length=.1,angle=20)
text(-10,0.08,bquote(paste(alpha," = ",italic(P),"(",bar(italic(X))<=-2.68,") = ",.(pval))),cex=cex3)
mtext("(d)",3,at=-15.5,line=-1.6,cex=cex1)

Power.graph.lower (x= -2.677955,sigma=45, n=200,to=15,from=-15,mu=-7,xlm=c(-16,15),xtext=TRUE, cex.axis = cex2)
arrows(5,0.06,-7,0.02,length=.1,angle=20)
pval<-round(pnorm(-2.677955,-7,45/sqrt(200)),2)
text(5,0.07,bquote(paste("1-",beta," = ",italic(P),"(",bar(italic(X))<=-2.68,") = ",.(pval))),cex=cex3)
mtext(expression(bar(italic(x))),1,at=-18,line=4,cex=1.2)
}
 
Fig.6.8<-function(){
par(mar=c(5,4.5,1,1))
plant.count <- c(1, 3, 4, 1, 1, 2, 1, 6, 6, 6, 1, 4, 7, 3, 1, 3, 2, 3, 0, 4, 1, 3, 5, 1, 1, 4, 1, 3, 2, 4) 
e <- ecdf(plant.count)
plot(e, main = " ", xlim = c(0,7.5), ylab = expression(paste(italic(Fn),"(",italic(x),")")), xlab = expression(italic(x)), cex.axis = 1.3, cex.lab = 1.3, cex = 1.4)
points(plant.count + 1,e(plant.count),cex = 1.2)
F.x <- punif(seq(0, 8), 0, 8)
points(seq(0, 8), F.x, pch = 19, col = "gray", cex = 1.4)
points(seq(1,9), F.x, col = "gray", cex = 1.4)
segments(seq(0, 8), F.x, seq(1, 9), F.x, col = "gray")

arrows(4.5, F.x[5], 4.5, e(4), code = 3, lwd = 3)
text(4.9, mean(c(F.x[5], e(4))), "  0.333", cex = 1.3) 
legend("topleft", legend = c("ecdf", "UNIF(0, 8)"), col = c(1, "gray"), lty = c(1, 1), inset = 0.02, bty = "n", cex = 1.3)
}


# Fig. 6.9
data(myeloma)
attach(myeloma)
c <- mglobulin[drug=="Control"]
t <- mglobulin[drug=="Trt"]

dev.new(height = 3.5)
par(mfrow = c(1,2),mar = c(4.4, 4, 0.5, 0.5), bg = "white")
cq <- qqnorm(c, plot.it = F)
tq <- qqnorm(t, plot.it = F)
plot(c(cq$x, tq$x), c(cq$y, tq$y), type = "n", xlab = "Theoretical quantiles", ylab = "Sample quantiles")
points(cq$x, cq$y, cex = 1)
points(tq$x, tq$y, pch = 19, cex = 1)
qqline(c)
qqline(t, lty = 2)
legend("topleft", pch = c(1, 19), lty = c(1, 2), legend = c("Control", "Treatment"), bty = "n") 
hist(c, main = "",xlab = "Microglobulin level (mg/l)", xlim = c(0, 10), density = 20, angle = 45)
hist(t, main = "",xlab = "", add= T,, density = 20, angle = 135)
legend("topright", legend = c("Control", "Treatment"), bty = "n") 
rect(5.9,4.62,6.5,4.82,density=20, angle = 45)
rect(5.9,4.22,6.5,4.42,density=20, angle = 135)
