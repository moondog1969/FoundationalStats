Fig.7.1 <- function(){
install.packages("diagram")
library(diagram)

M <- matrix(nrow = 3, ncol = 3, byrow = TRUE, data = 0)
M[1,2]<-"causal";M[3,2]<-"causal";M[3,1]<-"assoc"; M[1,3]<-1
col<-M
col[]<-"black"
col[3,1]<-"gray";col[1,3]<-"gray"
curv <- matrix(nrow = 3, ncol = 3, byrow = TRUE, data = 0)
curv[3,1]<-0.1;curv[1,3]<-0.1
c.txt<-matrix(nrow = 3, ncol = 3, byrow = TRUE, data = 1.5)
c.txt[1,3]<-0
par(mar=c(1,1,1,1))
plotmat(M, name = c("X", "Z", "Y"), box.size = 0.09, curve = curv, arr.pos = 0.5, arr.col = col, arr.lcol = col, cex.txt = 0, lwd = 3, box.cex=1.7, arr.length = 0.5, arr.width = 0.25)
}

Fig.7.2 <- function(){ 
M <- matrix(nrow = 3, ncol = 3, byrow = TRUE, data = 0)
M[1,2]<-"causal";M[2,1]<-"causal";M[3,2]<-"causal";M[3,1]<-1
col<-M
col[]<-"black"
col[1,2]<-"gray";col[2,1]<-"gray"
curv <- matrix(nrow = 3, ncol = 3, byrow = TRUE, data = 0)
curv[1,2]<-0.1;curv[2,1]<-0.1
c.txt<-matrix(nrow = 3, ncol = 3, byrow = TRUE, data = 1.5)
c.txt[2,3]<-0
par(mar=c(1,1,1,1))
plotmat(M, name = c("X", "Z", "Y"), box.size = 0.09, curve = curv, arr.pos = 0.5, arr.col = col, arr.lcol = col, cex.txt = 0 , lwd = 3, box.cex=1.7, arr.length = 0.5, arr.width = 0.25)
text(0.25,0.6,"?",cex=2)
text(0.75,0.6,"?",cex=2)
}

# Fig.7.3

anm.samp.design(lcol = gray(.4), lwd = 1.5)
mtext(side=3, "(a)", at = 0.024, line = -1.5, cex = 1.5, outer = T)
mtext(side=3, "(b)", at = 0.516, line = -1.5, cex = 1.5, outer = T)
mtext(side=3, "(c)", at = 0.024, line = -22.5, cex = 1.5, outer = T)
mtext(side=3, "(d)", at = 0.516, line = -22.5, cex = 1.5, outer = T)


Fig.7.4<-function(){
dev.new(height=3.5, width=7)
par(mfrow=c(1,2),mar=c(0,1,2,1.5), oma = c(4.5,3.5,0,0), bg = "white")

curve(dnorm(x,mean=1.01,sd=0.04),from=0.8,to=1.2,main=expression(paste(italic(X)," ~ ",italic(N),"(1.01,0.0016)")),xlab="", cex.main=.9)
mtext(side=2,expression(paste(italic(f),"(",hat(lambda),")")), outer=T, line = 1.8, cex= 1.1)
mtext(side=1,expression(hat(lambda)),outer=T, line = 3, cex= 1.1)
polygon(c(seq(0.8,1,.01),1),c(dnorm(seq(0.8,1,.01),1.01,0.04),dnorm(0.8,1.01,.04)), col="gray")

arrows(0.9,4,0.95,1,length=.1,angle=20)
text(0.87,4.3, expression(paste(italic(P),"(",italic(X)<=1,") = 0.4")),cex=.9)
curve(dnorm(x,mean=1.01,sd=0.004),from=0.99,to=1.03, main=expression(paste(italic(X)," ~ ",italic(N),"(1.01,0.000016)")),xlab="",ylab = "",cex.main=.9)
polygon(c(seq(0.99,1,.01),1),c(dnorm(seq(0.99,1,.01),1.01,0.004),dnorm(0.99,1.01,.004)), col="gray")
arrows(0.999,37,0.999,1,length=.1,angle=20)
text(0.999,40, expression(paste(italic(P),"(",italic(X)<=1,") = 0.006")),cex=.9)
}



plt.stl <- function (x, labels = colnames(X), set.pars = list(mar = c(0, 
    6, 0, 6), oma = c(6, 0, 4, 0), tck = -0.01, mfrow = c(nplot, 
    1)), main = NULL, range.bars = TRUE, ..., col.range = "light gray") 
{
    sers <- x$time.series
    ncomp <- ncol(sers)
    data <- drop(sers %*% rep(1, ncomp))
    X <- cbind(data, sers)
    colnames(X) <- c("data", colnames(sers))
    nplot <- ncomp + 1
    if (range.bars) 
        mx <- min(apply(rx <- apply(X, 2, range), 2, diff))
    dev.hold()
    on.exit(dev.flush())
    if (length(set.pars)) {
        oldpar <- do.call("par", as.list(names(set.pars)))
        on.exit(par(oldpar), add = TRUE)
        do.call("par", set.pars)
    }
    for (i in 1L:nplot) {
        plot(X[, i], type = if (i < nplot) 
            "l"
        else "h", xlab = "", ylab = "", axes = FALSE, ...)
        if (range.bars) {
            dx <- 1/64 * diff(ux <- par("usr")[1L:2])
            y <- mean(rx[, i])
            rect(ux[2L] - dx, y + mx/2, ux[2L] - 0.4 * dx, y - 
                mx/2, col = col.range, xpd = TRUE)
        }
        if (i == 1 && !is.null(main)) 
            title(main, line = 2, outer = par("oma")[3L] > 0)
        if (i == nplot) 
            abline(h = 0)
        box()
        right <- i%%2 == 0
        axis(2, labels = !right)
        axis(4, labels = right)
        axis(1, labels = i == nplot)
        mtext(labels[i], side = 2, 3)
    }
    mtext("Time", side = 1, line = 3)
    invisible()
}



data(PM2.5)
pmts <- ts(PM2.5[,2], start = c(1998, 11),frequency = 12)
plt.stl(stl(pmts, s.window = "periodic"),set.pars = list(mar = c(0, 6, 0, 3), oma = c(6, 0, 2, 0),mfrow = c(4, 1)),labels = c("Data", "Seasonal","Trend","Error")) 

# Fig 7.5
par(cex=1.3)
X <- seq(1,20); Y <- c(seq(1,7), rep(7,6), seq(8,14)) + rnorm(20, sd = 0.5)
plot(X, Y, xlab = expression(italic(X)), ylab = expression(italic(Y)))

# Fig 7.9
ExpDesign(method = c("CRD","factorial2by2","nested","RCBD","split","pairs"),cex.text=1.4, mp.col= 1)


Fig.7.10<-function(){
s<-seq(1,100)/100
x<-rep(s,100)
y<-rep(s,each=100)
dev.new(height=3.5,width=7)
par(mfrow=c(1,2),mar=c(0,0,0,0))

plot(x,y,cex=1,pch=16,col=gray(y),xaxt="n",yaxt="n",xlab="",ylab="")
text(.09,.93,"(a)",cex=1.5)
arrows(.5,.5,.5,.6,length=.2)
text(.55,.55,"Gradient",srt=90)

rect(.35,.15,.55,.25)
text(.45,.2,"Block 1")
rect(.45,.35,.65,.45)
text(.55,.4,"Block 2")
rect(.35,.65,.55,.75)
text(.45,.7,"Block 3")
rect(.45,.85,.65,.95)
text(.55,.9,"Block 4")

plot(x,y,cex=1,pch=16,col=gray(x),xaxt="n",yaxt="n",xlab="",ylab="")
text(.09,.93,"(b)",cex=1.5,col = "white")
arrows(.4,.55,.6,.55,length=.2)
text(.5,.5,"Gradient")

rect(.35,.15,.55,.25)
text(.45,.2,"Block 1")
rect(.45,.35,.65,.45)
text(.55,.4,"Block 2")
rect(.35,.65,.55,.75)
text(.45,.7,"Block 3")
rect(.45,.85,.65,.95)
text(.55,.9,"Block 4")
}



 



