data(potato)
anova(lm(Yield ~ Variety * Fert, data = potato))


Fig.10.1 <- function(){
par(mar=c(8.5,4,2,1), cex=1.2)
with(potato, interaction.plot(Variety, Fert, Yield, trace.label = "Fertilizer", las = 2, xlab = "", legend = F, ylab = "Mean yield (lbs/plant)"))
legend("bottomright", lty = c(3,2,1), legend = c("B", "Cl", "S"), bty = "n", title = "Fertilizer", inset = .05)
}

Fig.10.3<-function(){
data(baby.walk)
s <- scheffe.cont(baby.walk$date, baby.walk$treatment, lvl = c("AE", "TO"))
sr <- diff(range(c(s$"Lower", s$"Upper")))
sr <- rep(sr,15)
ar <- seq(1:15)

for(i in 1:15){
b <- bonf.cont(baby.walk$date, baby.walk$treatment, lvl = c("AE", "TO"), comps = i)
ar[i] <- diff(range(c(b$"Lower", b$"Upper")))
}

par(cex=1.3, mar = c(5,4.5, 2,2))
plot(seq(1,15),ar,type = "n", xlab = "Number of comparisons", ylab = "95% confidence interval width")
lines(seq(1,15), ar, lty = 1, lwd = 2)
lines(seq(1,15), sr, lty = 2, lwd = 2)
legend("bottomright", lty = c(1,2), legend=c("Bonferroni", "Scheffe"), inset = .01, bty = "n", lwd=2)
} 

## Fig 10.4
dev.new(height=3.5)
par(mfrow = c(1, 2), mar = c(4,4,2,1))
plot(pairw.anova(baby.walk$date, baby.walk$treatment), main = "", las = 2)
bplot(baby.walk$date, baby.walk$treatment, simlett=TRUE, lett=c("b","a","ab","ab"), ylab = "Onset of walking (months from birth)", cex.lett = 0.9)

## Fig 10.6
baby.lm <- lm(date ~ treatment, data = baby.walk)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1.5)) 
plot(baby.lm, col.smooth="gray")  
mtext("(a)",3,outer = TRUE, at = .05, line = -1.5, cex = 1.4)
mtext("(b)",3,outer = TRUE, at = .55, line = -1.5, cex = 1.4)
mtext("(c)",3,outer = TRUE, at = .05, line = -22, cex = 1.4)
mtext("(d)",3,outer = TRUE, at = .55, line = -22, cex = 1.4)


Fig10.7 <- function(){
library(lattice); library(grid)
trellis.device(color=F)

p1 <- plot(ref.K, cex=.7, grid = F, id = .05, adj = -0.5, xlab=grid.text("Fitted values",gp=gpar(fontsize=9)),ylab= grid.text("Standardized residuals", gp=gpar(fontsize=9),rot=90))
p2 <- qqnorm(ref.K, cex=.7,main="", id = .05, adj = -0.5, ylab=grid.text("Quantiles of standard normal",gp=gpar(fontsize=9),rot=90),xlab= grid.text("Standardized residuals", gp=gpar(fontsize=9)))
p3 <- plot(ref.K, cex=.6, resid(., type = "p") ~ fitted(.) | lab, abline = 0, grid = F, xlab=grid.text("Fitted values",gp=gpar(fontsize=9)),ylab= grid.text("Standardized residuals", gp=gpar(fontsize=9),rot=90))
p4 <- qqnorm(ref.K, ~ ranef(.), cex=.7, grid = F, id = .05, adj = -0.5, ylab=grid.text("Quantiles of standard normal",gp=gpar(fontsize=9),,rot=90),xlab= grid.text("Standardized residuals", gp=gpar(fontsize=9)))


print(p1, split=c(1,1,2,2), more=TRUE)#tl
print(p2, split=c(2,1,2,2), more=TRUE)#tr
print(p3, split=c(1,2,2,2), more=TRUE)#bl
print(p4, split=c(2,2,2,2), more=FALSE)#br
ltext(15,490,"(a)",cex=1.4)
ltext(15,260,"(c)",cex=1.4)
ltext(250,490,"(b)",cex=1.4)
ltext(260,260,"(d)",cex=1.4)
}

Fig.10.9<-function(){
par(mfrow=c(2,2))
par(mar=c(1.5,4.2,2.2,0.5))
##(a)
biomass<-c(12.7,13.7,14.7,15.7,16.7,17.7,13,14,15,16,17,18)
Fert<-rep(c("Co","Co","Co","N+","N+","N+"),2)
Water<-rep(c("Dry","Mesic","Wet"),4)
interaction.plot(x.factor=Water,trace.factor=Fert,
response=biomass,ylab="Mean biomass",xlab="",xaxt="n",legend=FALSE, cex.lab=1.2)
legend("bottomright",title="Fert",lty=c(1,2),legend=c("N+","Co"),bg="white",box.col="white",cex=1.1,inset=.01)
legend("topleft",legend=c(expression(paste("A  ",italic(P),"=3.0e-07")),expression(paste("B  ",italic(P),"=3.5e-05")),expression(paste("A:B  ",italic(P),"=1"))),bty="n",cex=1)
#title=expression(underline("ANOVA results"))
##(b)
par(mar=c(1.5,2.5,2.2,1.2))
biomass<-c(14.6,15.1,14.7,16.2,16.7,16.6,14.8,15.2,15,16.6,17,17.2)
Fert<-rep(c("Co","Co","Co","N+","N+","N+"),2)
Water<-rep(c("Dry","Mesic","Wet"),4)
interaction.plot(x.factor=Water,trace.factor=Fert,
response=biomass,ylab="Mean biomass",legend=FALSE,xlab="",xaxt="n", cex.lab=1.2)
#legend("right",title="Fert",lty=c(1,2),legend=c("N+","Co"),bg="white",box.col="white",cex=1.1,inset=.01)
legend("right",legend=c(expression(paste("A  ",italic(P),"=1.5e-05")),expression(paste("B  ",italic(P),"=0.1")),expression(paste("A:B  ",italic(P),"=0.55"))),bty="n",cex=1,y=16, inset=.1) 
#title=expression(underline("ANOVA results"))
##(c)
par(mar=c(4.2,4.2,0,0.5))
biomass<-c(14.6,15.1,14.7,16.2,16.7,12.6,14.8,15.2,15,16.6,17,12.2)
Fert<-rep(c("Co","Co","Co","N+","N+","N+"),2)
Water<-rep(c("Dry","Mesic","Wet"),4)
interaction.plot(x.factor=Water,trace.factor=Fert,
response=biomass,ylab="Mean biomass",legend=FALSE, cex.lab =1.2)
#legend("topright",title="Fert",lty=c(1,2),legend=c("N+","Co"),bg="white",box.col="white",cex=1.1,inset=.01)
legend("bottomleft",legend=c(expression(paste("A  ",italic(P),"=0.04")),expression(paste("B  ",italic(P),"= 9.5e-06")),expression(paste("A:B  ",italic(P),"=1.3e-05"))),bty="n",cex=1) 
#title=expression(underline("ANOVA results"))
##(d)
par(mar=c(4.2,2.5,0,1.2))
biomass<-c(14.6,15.1,12.4,16.2,16.7,17.6,14.8,15.2,12,16.6,17,17.2)
Fert<-rep(c("Co","Co","Co","N+","N+","N+"),2)
Water<-rep(c("Dry","Mesic","Wet"),4)
interaction.plot(x.factor=Water,trace.factor=Fert,
response=biomass,ylab="Mean biomass",legend=FALSE,cex.lab=1.2)
#legend("right",title="Fert",lty=c(1,2),legend=c("N+","Co"),bg="white",box.col="white",cex=1.1,inset=.01)
legend("bottomleft",legend=c(expression(paste("A  ",italic(P),"=6.01e-07")),expression(paste("B  ",italic(P),"=8.67e-04")),expression(paste("A:B  ",italic(P),"=4.89e-05"))),bty="n",cex=1) 
#title=expression(underline("ANOVA results")),

mtext("(a)",3,outer = TRUE, at = .025, line = -1.8, cex = 1.4)
mtext("(b)",3,outer = TRUE, at = .52, line = -1.8, cex = 1.4)
mtext("(c)",3,outer = TRUE, at = .025, line = -21, cex = 1.4)
mtext("(d)",3,outer = TRUE, at = .52, line = -21, cex = 1.4)


}


##############################################

Fig.10.13 <- function(){
Biom.test<-data.frame(Treatment=factor(c(rep(1,7),rep(2,7))),Scores=c(111,112,112,113,117,116,116,117,119,121,122,121,123,127),Studytime=c(20,23,20,21,24,22,23,27,28,26,28,29,31,31))
par(cex = 1.3)
plot(Scores~Studytime,data=Biom.test,pch=c(rep(1,7),rep(19,7)), xlab= "Study time (hours)",ylab="Standardized score")
abline(87.8979+ 0.1747, 1.1797,col=1)
abline(87.8979- 0.1747, 1.1797,col=1, lty = 2)
legend("topleft", pch=c(1,19),lty=c(1,2),legend=c("Online Biometry Course", "Conventional Biometry Course")) 
}

################################################



Fig.10.14 <- function(){
data(fly.sex)
par(cex = 1.2)
y.int <- c(0.16523, 0.08123, 0.13326, -0.04268, -0.395) + 3.99678   
sy <- replace(as.numeric(fly.sex$treatment), fly.sex$treatment == 5, 19) 
thorax1 <- fly.sex$thorax - mean(fly.sex$thorax)

pdfFonts(stdsym=Type1Font("standardsymbol",
            c("Helvetica.afm",
              "s050000l.afm",
              "Helvetica-Oblique.afm",
              "Helvetica-Bold.afm",
              "Symbol.afm"),
            encoding="CP1253"))


with(fly.sex, plot(thorax1, log(longevity), xlab = "Centered thorax measures (cm)", ylab = expression(paste("Age  ", log[e], "(days)", sep = "")), pch = sy, cex = .8))
for(i in 1:5) abline(y.int[i], 2.75, lty = i)
points(c(0.03785, 0.03785, 0.03785, 0.03785, 0.03785) , c(3.188, 3.09, 2.993, 2.895, 2.798) , pch= c(1, 2, 3, 4, 19)) 
segments(c(0.02727,0.02727,0.02727,0.02727,0.02727), c(3.188,3.09, 2.993,2.895, 2.798),c(0.02727,0.02727,0.02727,0.02727,0.02727)+0.02116,c(3.188,3.09, 2.993,2.895, 2.798),lty=1:5) 
text(c(0.02727,0.02727,0.02727,0.02727,0.02727)+0.025, c(3.188,3.09, 2.993,2.895, 2.798), c("1 virgin", "8 virgin", "1 pregnant","8 pregnant", "None"),pos=4, family="stdsym")
}

