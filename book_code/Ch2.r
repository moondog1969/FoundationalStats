Fig.2.1<-function(){
data(webs)#Points estimated from a graph in Gosline 1984
w<-webs
dev.new(height=5,width=8.5)
par(mfrow=c(1,2))
plot(w[,3],w[,2],xlab="Temp. (\u00b0C)",ylab="Relative length")

ce<-w[w[,3]<=30,]
llm<-lm(ce[,2]~ce[,3])
c<-coef(llm)
r<-resid(llm)
abline(c[1],c[2])

hist(r,breaks=8,main="",xlab="Residuals (Distance of points above or below fit line)",density=30)
mtext("(b)", side = 3, at = -0.00125, cex = 1.2)

mtext("(a)", side = 3, at = -0.00368, cex = 1.2)
}
##############################################################

