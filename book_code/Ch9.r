Fig.9.1<-function(){
dev.new(height=3.5)
par(mfrow=c(1,2),mar=c(5,4.3,1.5,0.15))
birds<-data.frame(loc=c("Greenland","Labrador","Newfoundland","New York","Florida","Guatamala","Panama","Columbia"),
lat=c(71,52.5,50,37,32,15,11,5), no.sp=c(56,81,118,195,143,469,1100,1395))
attach(birds)
plot(lat,logb(no.sp,10),xlab="Latitude (degrees N)",ylab=expression(paste(log[10],"(Number of bird species)")),xlim=c(-5,90),ylim=c(1.7,3.3), cex=.95)
identify(lat,logb(no.sp,10),loc,cex=.7)
mtext(side=3,at=-22,"(a)", line=0,cex=1.4)
reg.bird<-lm(logb(no.sp,10)~lat)
abline(reg.bird$coefficients[1],reg.bird$coefficients[2])

brad<-data.frame(BSA=c(2,4,8,12,16),A595=c(0.01,0.18,0.43,0.58,0.76))
attach(brad)
plot(BSA,A595,xlab=expression(paste("BSA concentration (",mu,"g/ml)")),ylab=expression(A[595]),cex=.95)
reg.brad<-lm(A595~BSA)
abline(reg.brad$coefficients[1],reg.brad$coefficients[2])
mtext(side=3,at=-.4,"(b)", line=0,cex=1.4)
}

###################################################################

Fig.9.2 <- function(){
x <- c(.2, 1, 2, 3, 4.1, 4.4)
y <-c(2.973580+.2, 1.457149-.2, 2.457323-.2, 5.101700-.2, 7.276465+.2, 8.013580+.2)

par(cex=1.15)
plot(x, y, xlab = expression(italic(X)), ylab = expression(italic(Y)),ylim=c(0,max(y)), xlim=c(0,max(x)))
abline(0.6, 1.2, lwd = 2)

l <- lm(y~x)
abline(l$coefficients[1],l$coefficients[2], lty=2, col=gray(.4))

segments(3,  .6 + (1.2*3), 4,  .6 + (1.2*3))
segments(4, .6 + (1.2*3), 4,  .6 + (1.2*4))

segments(4.1,  .6 + (1.2*3), 4.1,  .6  + (1.2*4))
segments(4.07, .6 + (1.2*3), 4.13,  .6  + (1.2*3))
segments(4.07, .6 + (1.2*4), 4.13, .6  + (1.2*4))
segments(3,  .6  + (1.2*3)-.2, 4, .6  + (1.2*3)-.2)
segments(3,  .6  + (1.2*3)-.27, 3,  .6 + (1.2*3)-.13)
segments(4,  .6  + (1.2*3)-.27, 4,  .6 + (1.2*3)-.13)
m1 <- mean(c(.6 + (1.2*3),  .6  + (1.2*4)))
rect(4.05,m1-.25,4.15,m1+.25,col="white",border="white")
text(4.1,m1, expression(beta[1]))
rect(3.2,.6 + (1.2*3)-.15,3.8,.6 + (1.2*3)-.45,col="white",border="white")
text(3.5,.6 + (1.2*3)-.2,expression(paste(Delta,italic(X)," = 1")))
segments(0,  0, 0,  .6)
segments(-.03,  0, .03,  0)
segments(-.03,  0.6, .03,  0.6)
text(0.14,.3,expression(beta[0]))

segments(.2,l$coefficients[1]+.2*l$coefficients[2],.2,y[1],lty=3,col=grey(.4))
segments(x[3],.6+1.2*x[3],x[3],y[3],lty=3)
text(.34,mean(c(l$coefficients[1]+.2*l$coefficients[2],y[1])),expression(italic(hat(epsilon)[i])),font=3)
text(x[3]+.14,mean(c(.6+1.2*x[3],y[3])),expression(italic(epsilon[i])),cex=1.1,font=3)
legend("topleft",lty=c(1,2), col=c(1,gray(.4)), lwd=c(2,1), legend=c(expression(paste("True reg. line: ", italic(E),"(",italic(Y[i]),") = 0.6 + 1.2",italic(X[i]))),
expression(paste("Fitted line: ", italic(hat(Y[i]))," = 0.91 + 1.48",italic(X[i])))), bty="n", cex=.9)

#rect(6, 4 + (1.2*6), 6.25, 4 + (1.2*6.25), lty=2)
#arrows(6.25, 4 + (1.2*6), 6.5, 10,  length=.1,lwd=2, angle = 20)
#rect(6.5, 3, 8, 10, lty=2)
}

#########################################################################


#Fig.9.3
data(Fbird)
attach(Fbird)
par(cex=1.3)
plot(vol, freq, xlab = expression(paste("Estimated gular pouch size ( ",cm^3,")")),ylab = "Fundamental frequency (Hz)")
abline(beta.hat0, beta.hat1)


#Fig.9.4<-function(){
#alpine<-data.frame(CAPA=c(0.0,0.1,1.0,0.0,31.1,27.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,21.0,39.5,0.0,0.0,43.0,42.1,0.0,0.0,0.1,0.0,0.0,0.0,55.0,43.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0),
#CEAR=c(0.3,1.4,7.7,4.5,3.6,1.1,0.0,0.0,5.0,4.8,1.0,0.6,0.5,2.5,5.7,3.0,7.2,2.0,2.5,1.9,2.2,3.5,2.3,1.6,1.3,0.4,7.0,3.8,0.0,0.2,2.7,3.3,7.6,24.0,0.0,2.3,14.8,16.1,11.3,5.5),
#hot.days=c(51.4,51.4,0.0,0.0,0.0,0.0,69.4,66.9,29.5,29.5,47.2,47.2,55.4,55.4,42.5,42.5,37.1,37.1,30.8,30.8,58.6,58.6,44.8,44.8,65.8,65.8,78.1,78.1,70.7,70.7,0.0,0.0,34.0,34.0,62.1,62.1,35.8,35.8,70.8,70.8),
#wet.days=c(8.9,8.9,7.2,7.2,22.3,22.3,10.0,19.9,11.3,11.3,16.9,16.9,6.1,6.1,6.6,6.6,8.8,8.8,23.3,23.3,6.5,6.5,18.6,18.6,8.1,8.1,10.7,10.7,6.6,6.6,18.0,18.0,7.7,7.7,36.7,37.7,7.1,7.1,7.7,7.7))
#attach(alpine)
#mreg.capa<-lm(CAPA~hot.days+wet.days)
#library(scatterplot3d)
#dev.new(height=3.5)
#par(mfrow=c(1,2), cex.lab=.8,cex.axis=.8)
#s3d1<-scatterplot3d(cbind(hot.days,wet.days,CAPA),mar=c(3,2.5,0,1.8), type="p", highlight.3d=T,angle=55,
#scale=0.7, pch=16,xlab=expression(paste("Days with soil >  ",10^o, "C")),
#ylab=expression(paste("Days with  ", Psi[soil], " > -1.5 MPa")), zlab="",box=F,col.grid=gray(.9))
#mtext(expression(paste("Percent cover of ", italic("Carex paysonis"))),2,cex=.8,line=1)
#s3d1$plane3d(mreg.capa,lty=1,col=gray(.5))
#mreg.cear<-lm(CEAR~hot.days+wet.days)
#s3d2<-scatterplot3d(cbind(hot.days,wet.days,CEAR), mar=c(3,2.5,0,1.8),type="p", highlight.3d=T,angle=55,
#scale=0.7, pch=16,xlab=expression(paste("Days with soil >  ",10^o, "C")),
#ylab=expression(paste("Days with  ", Psi[soil], " > -1.5 MPa")), zlab="",box=F,col.grid=gray(.9))
#mtext(expression(paste("Percent cover of ", italic("Cerastium arvense"))),2,cex=.8,line=1)
#s3d2$plane3d(mreg.cear,lty=1,col=gray(.5))
#}
#

Fig.9.5<-function(){
alpine<-data.frame(CAPA=c(0.0,0.1,1.0,0.0,31.1,27.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,21.0,39.5,0.0,0.0,43.0,42.1,0.0,0.0,0.1,0.0,0.0,0.0,55.0,43.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0),
CEAR=c(0.3,1.4,7.7,4.5,3.6,1.1,0.0,0.0,5.0,4.8,1.0,0.6,0.5,2.5,5.7,3.0,7.2,2.0,2.5,1.9,2.2,3.5,2.3,1.6,1.3,0.4,7.0,3.8,0.0,0.2,2.7,3.3,7.6,24.0,0.0,2.3,14.8,16.1,11.3,5.5),
hot.days=c(51.4,51.4,0.0,0.0,0.0,0.0,69.4,66.9,29.5,29.5,47.2,47.2,55.4,55.4,42.5,42.5,37.1,37.1,30.8,30.8,58.6,58.6,44.8,44.8,65.8,65.8,78.1,78.1,70.7,70.7,0.0,0.0,34.0,34.0,62.1,62.1,35.8,35.8,70.8,70.8),
wet.days=c(8.9,8.9,7.2,7.2,22.3,22.3,10.0,19.9,11.3,11.3,16.9,16.9,6.1,6.1,6.6,6.6,8.8,8.8,23.3,23.3,6.5,6.5,18.6,18.6,8.1,8.1,10.7,10.7,6.6,6.6,18.0,18.0,7.7,7.7,36.7,37.7,7.1,7.1,7.7,7.7))
attach(alpine)
mreg.capa<-lm(CAPA~hot.days+wet.days)
library(scatterplot3d)
par(cex.lab=1.2,cex.axis=1.2)
s3d1<-scatterplot3d(cbind(hot.days,wet.days,CAPA),mar=c(4,3.5,0,3), type="h", highlight.3d=F,angle=55,
scale=0.7, xlab=expression(paste("Days with soil > ",10^o, "C")),
ylab=expression(paste("Days with  ", Psi[soil], " > -1.5 MPa")), zlab="",box=F,col.grid=gray(.9), cex.symbols=1.2, lty.hplot=2)
mtext(expression(paste("Percent cover of ", italic("Carex paysonis"))),2,cex=1.2,line=1.3)
s3d1$plane3d(mreg.capa,lty=1,col=gray(.5))
}

#Fig.9.6
par(cex=1.3)
with(Fbird, plotCI.reg(vol, freq, xlab = expression(paste("Gular pouch size ( ",cm^3,")")),ylab = "Fundamental frequency (Hz)"),CI.col="gray",PI.col="gray")

  
######################################################################

Fig.9.7<-function(){
require(scatterplot3d)
X<-Y<-0:20
Z<-seq(0,.6,.03)

par(cex.lab=1.2, cex.axis=1.2)
e<-scatterplot3d(X,Y,Z,scale.y=1.75,type="n",angle=50,xlab=expression(italic(x)),ylab=expression(italic(y)),
zlab=expression(paste(italic(f),"(",italic(y),")", sep = "")),lty.hide=0,box=FALSE, mar = c(3, 3, 0, 3), ylim = c(0,17))
e$points3d(seq(0,20),(.8*seq(0,20))+1.3, rep(0,21),type="l",lwd=2)
e$points3d(rep(3,21),seq(0,20),rep(0,21),type="l",col="gray",lty=2)
e$points3d(seq(0,20),rep(3.7,21),rep(0,21),type="l",col="gray",lty=2)
e$points3d(rep(3,61),seq(0.7,6.7,.1),dnorm(seq(0.7,6.7,.1),3.7,1),type="l")
e$points3d(rep(3,61),seq(0.7,6.7,.1),rep(0,61),type="l")
e$points3d(rep(3,400),rep(3.7,400),seq(0,round(dnorm(3.7,3.7,1),3),.001),type="l")
e$points3d(rep(14,61),seq(9.5,15.5,.1),dnorm(seq(9.5,15.5,.1),12.5,1),type="l")
e$points3d(seq(0,20),rep(12.5,21),rep(0,21),type="l",col="gray",lty=2)
e$points3d(rep(14,21),seq(0,20),rep(0,21),type="l",col="gray",lty=2)
e$points3d(rep(14,61),seq(9.5,15.5,.1),rep(0,61),type="l")
e$points3d(rep(14,400),rep(12.5,400),seq(0,round(dnorm(12.5,12.5,1),3),.001),type="l")


arrows(3.5,6.8,2.8,1.8,length=.1)
text(3.5,7.2, expression(paste(italic(E),"(",italic(Y),") = 1.3 + 0.8",italic(X),sep="")), cex=1.2)
arrows(6,1.5,4.8,2.38,length=.1)
text(6.7,1.2,expression(paste(italic(E),"(",italic(Y[i]),") = 12.5")), cex=1.2)
e$points3d(14,11,0,pch=16)
e$points3d(3,4.2,0,pch=16)
arrows(5.85,.65,4.55,2.08,length=.1)
text(6.3,.33,expression(paste(italic(Y[i])," = 11")), cex=1.2)
text(6.3,-.22,expression(paste(italic(epsilon[i])," = ",italic(Y[i])," - ", italic(E),"(",italic(Y[i]),") = -0.5")), cex=1.2)
}

## For additional clarification, try:
curve(dnorm(x,4,1.5),from=-7,to=11,xlab="x", ylab="f(x)",lwd=2,cex.axis=1.1,cex.lab=1.2,ylim=c(0,0.32))
curve(dnorm(x,0,1.5),from=-7,to=11,add=T,col="gray",lwd=2,cex=1.2)
segments(0,0,0,dnorm(0,0,1.5),lty=2,col="gray")
segments(4,0,4,dnorm(4,4,1.5),lty=2)
legend("topleft",col=c("black","gray"),lwd=2,legend=c(expression(paste(Y[i]," ~ N(",mu[i],", ", sigma^2,")")),
expression(paste(epsilon[i]," ~ N(0, " ,sigma^2,")"))),bg="white",box.col="white",cex=1.2,inset=.002)
arrows(4, 0.132, 0, 0.132, length = 0.1, lwd =5)
legend("center",legend=expression(paste(epsilon[i]," = ",Y[i]," - ",mu[i])),bg="white",box.col="white",cex=1.2)


##########################################################################################


plot.lm <- function (x, which = c(1L:3L, 5L), caption = list("Residuals vs Fitted", 
    "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", 
    expression("Cook's dist vs Leverage  " * h[ii]/(1 - h[ii]))), 
    panel = if (add.smooth) panel.smooth else points, sub.caption = NULL, 
    main = "", ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
    ..., id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75, 
    qqline = TRUE, cook.levels = c(0.5, 1), add.smooth = getOption("add.smooth"), 
    label.pos = c(4, 2), cex.caption = 1) 
{
    dropInf <- function(x, h) {
        if (any(isInf <- h >= 1)) {
            warning("Not plotting observations with leverage one:\n  ", 
                paste(which(isInf), collapse = ", "), call. = FALSE)
            x[isInf] <- NaN
        }
        x
    }
    if (!inherits(x, "lm")) 
        stop("use only with \"lm\" objects")
    if (!is.numeric(which) || any(which < 1) || any(which > 6)) 
        stop("'which' must be in 1:6")
    isGlm <- inherits(x, "glm")
    show <- rep(FALSE, 6)
    show[which] <- TRUE
    r <- residuals(x)
    yh <- predict(x)
    w <- weights(x)
    if (!is.null(w)) {
        wind <- w != 0
        r <- r[wind]
        yh <- yh[wind]
        w <- w[wind]
        labels.id <- labels.id[wind]
    }
    n <- length(r)
    if (any(show[2L:6L])) {
        s <- if (inherits(x, "rlm")) 
            x$s
        else if (isGlm) 
            sqrt(summary(x)$dispersion)
        else sqrt(deviance(x)/df.residual(x))
        hii <- lm.influence(x, do.coef = FALSE)$hat
        if (any(show[4L:6L])) {
            cook <- if (isGlm) 
                cooks.distance(x)
            else cooks.distance(x, sd = s, res = r)
        }
    }
    if (any(show[2L:3L])) {
        ylab23 <- if (isGlm) 
            "Std. deviance resid."
        else "Standardized residuals"
        r.w <- if (is.null(w)) 
            r
        else sqrt(w) * r
        rs <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
    }
    if (any(show[5L:6L])) {
        r.hat <- range(hii, na.rm = TRUE)
        isConst.hat <- all(r.hat == 0) || diff(r.hat) < 1e-10 * 
            mean(hii, na.rm = TRUE)
    }
    if (any(show[c(1L, 3L)])) 
        l.fit <- if (isGlm) 
            "Predicted values"
        else "Fitted values"
    if (is.null(id.n)) 
        id.n <- 0
    else {
        id.n <- as.integer(id.n)
        if (id.n < 0L || id.n > n) 
            stop(gettextf("'id.n' must be in {1,..,%d}", n), 
                domain = NA)
    }
    if (id.n > 0L) {
        if (is.null(labels.id)) 
            labels.id <- paste(1L:n)
        iid <- 1L:id.n
        show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
        if (any(show[2L:3L])) 
            show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]
        text.id <- function(x, y, ind, adj.x = TRUE) {
            labpos <- if (adj.x) 
                label.pos[1 + as.numeric(x > mean(range(x)))]
            else 3
            text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE, 
                pos = labpos, offset = 0.25)
        }
    }
    getCaption <- function(k) if (length(caption) < k) 
        NA_character_
    else as.graphicsAnnot(caption[[k]])
    if (is.null(sub.caption)) {
        cal <- x$call
        if (!is.na(m.f <- match("formula", names(cal)))) {
            cal <- cal[c(1, m.f)]
            names(cal)[2L] <- ""
        }
        cc <- deparse(cal, 80)
        nc <- nchar(cc[1L], "c")
        abbr <- length(cc) > 1 || nc > 75
        sub.caption <- if (abbr) 
            paste(substr(cc[1L], 1L, min(75L, nc)), "...")
        else cc[1L]
    }
    one.fig <- prod(par("mfcol")) == 1
    if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    if (show[1L]) {
        ylim <- range(r, na.rm = TRUE)
        if (id.n > 0) 
            ylim <- extendrange(r = ylim, f = 0.08)
        dev.hold()
        plot(yh, r, xlab = l.fit, ylab = "Residuals", main = main, 
            ylim = ylim, type = "n", ...)
        panel(yh, r, ...)
        if (one.fig) 
            title(sub = sub.caption, ...)
        mtext(getCaption(1), 3, 0.25, cex = cex.caption)
        if (id.n > 0) {
            y.id <- r[show.r]
            y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
            text.id(yh[show.r], y.id, show.r)
        }
        abline(h = 0, lty = 3, col = "gray")
        dev.flush()
    }
    if (show[2L]) {
        ylim <- range(rs, na.rm = TRUE)
        ylim[2L] <- ylim[2L] + diff(ylim) * 0.075
        dev.hold()
        qq <- qqnorm(rs, main = main, ylab = ylab23, ylim = ylim, 
            ...)
        if (qqline) 
            qqline(rs, lty = 3, col = "gray50")
        if (one.fig) 
            title(sub = sub.caption, ...)
        mtext(getCaption(2), 3, 0.25, cex = cex.caption)
        if (id.n > 0) 
            text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
        dev.flush()
    }
    if (show[3L]) {
        sqrtabsr <- sqrt(abs(rs))
        ylim <- c(0, max(sqrtabsr, na.rm = TRUE))
        yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))))
        yhn0 <- if (is.null(w)) 
            yh
        else yh[w != 0]
        dev.hold()
        plot(yhn0, sqrtabsr, xlab = l.fit, ylab = yl, main = main, 
            ylim = ylim, type = "n", ...)
        panel(yhn0, sqrtabsr, ...)
        if (one.fig) 
            title(sub = sub.caption, ...)
        mtext(getCaption(3), 3, 0.25, cex = cex.caption)
        if (id.n > 0) 
            text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
        dev.flush()
    }
    if (show[4L]) {
        if (id.n > 0) {
            show.r <- order(-cook)[iid]
            ymx <- cook[show.r[1L]] * 1.075
        }
        else ymx <- max(cook, na.rm = TRUE)
        dev.hold()
        plot(cook, type = "h", ylim = c(0, ymx), main = main, 
            xlab = "Obs. number", ylab = "Cook's distance", ...)
        if (one.fig) 
            title(sub = sub.caption, ...)
        mtext(getCaption(4), 3, 0.25, cex = cex.caption)
        if (id.n > 0) 
            text.id(show.r, cook[show.r], show.r, adj.x = FALSE)
        dev.flush()
    }
    if (show[5L]) {
        ylab5 <- if (isGlm) 
            "Std. Pearson resid."
        else "Standardized residuals"
        r.w <- residuals(x, "pearson")
        if (!is.null(w)) 
            r.w <- r.w[wind]
        rsp <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
        ylim <- range(rsp, na.rm = TRUE)
        if (id.n > 0) {
            ylim <- extendrange(r = ylim, f = 0.08)
            show.rsp <- order(-cook)[iid]
        }
        do.plot <- TRUE
        if (isConst.hat) {
            if (missing(caption)) 
                caption[[5L]] <- "Constant Leverage:\n Residuals vs Factor Levels"
            aterms <- attributes(terms(x))
            dcl <- aterms$dataClasses[-aterms$response]
            facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
            mf <- model.frame(x)[facvars]
            if (ncol(mf) > 0) {
                effM <- mf
                for (j in seq_len(ncol(mf))) effM[, j] <- sapply(split(yh, 
                  mf[, j]), mean)[mf[, j]]
                ord <- do.call(order, effM)
                dm <- data.matrix(mf)[ord, , drop = FALSE]
                nf <- length(nlev <- unlist(unname(lapply(x$xlevels, 
                  length))))
                ff <- if (nf == 1) 
                  1
                else rev(cumprod(c(1, nlev[nf:2])))
                facval <- (dm - 1) %*% ff
                xx <- facval
                dev.hold()
                plot(facval, rsp, xlim = c(-1/2, sum((nlev - 
                  1) * ff) + 1/2), ylim = ylim, xaxt = "n", main = main, 
                  xlab = "Factor Level Combinations", ylab = ylab5, 
                  type = "n", ...)
                grp_means <- sapply(split(yh, mf[, 1L]), mean)
                axis(1, at = ff[1L] * (order(grp_means) - 1/2) - 
                  1/2, labels = x$xlevels[[1L]])
                mtext(paste(facvars[1L], ":"), side = 1, line = 0.25, 
                  adj = -0.05)
                abline(v = ff[1L] * (0:nlev[1L]) - 1/2, col = "gray", 
                  lty = "F4")
                panel(facval, rsp, ...)
                abline(h = 0, lty = 3, col = "gray")
                dev.flush()
            }
            else {
                message("hat values (leverages) are all = ", 
                  format(mean(r.hat)), "\n and there are no factor predictors; no plot no. 5")
                frame()
                do.plot <- FALSE
            }
        }
        else {
            xx <- hii
            xx[xx >= 1] <- NA
            dev.hold()
            plot(xx, rsp, xlim = c(0, max(xx, na.rm = TRUE)), 
                ylim = ylim, main = main, xlab = "Leverage", 
                ylab = ylab5, type = "n", ...)
            panel(xx, rsp, ...)
            abline(h = 0, v = 0, lty = 3, col = "gray")
            if (one.fig) 
                title(sub = sub.caption, ...)
            if (length(cook.levels)) {
                p <- length(coef(x))
                usr <- par("usr")
                hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), 
                  usr[2L], length.out = 101)
                for (crit in cook.levels) {
                  cl.h <- sqrt(crit * p * (1 - hh)/hh)
                  lines(hh, cl.h, lty = 2, col = "gray50")
                  lines(hh, -cl.h, lty = 2, col = "gray50")
                }
                legend("bottomleft", legend = "Cook's distance", 
                  lty = 2, col = "gray50", bty = "n")
                xmax <- min(0.99, usr[2L])
                ymult <- sqrt(p * (1 - xmax)/xmax)
                aty <- c(-sqrt(rev(cook.levels)) * ymult, sqrt(cook.levels) * 
                  ymult)
                axis(4, at = aty, labels = paste(c(rev(cook.levels), 
                  cook.levels)), mgp = c(0.25, 0.25, 0), las = 2, 
                  tck = 0, cex.axis = cex.id, col.axis = "gray50")
            }
            dev.flush()
        }
        if (do.plot) {
            mtext(getCaption(5), 3, 0.25, cex = cex.caption)
            if (id.n > 0) {
                y.id <- rsp[show.rsp]
                y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
                text.id(xx[show.rsp], y.id, show.rsp)
            }
        }
    }
    if (show[6L]) {
        g <- dropInf(hii/(1 - hii), hii)
        ymx <- max(cook, na.rm = TRUE) * 1.025
        dev.hold()
        plot(g, cook, xlim = c(0, max(g, na.rm = TRUE)), ylim = c(0, 
            ymx), main = main, ylab = "Cook's distance", xlab = expression("Leverage  " * 
            h[ii]), xaxt = "n", type = "n", ...)
        panel(g, cook, ...)
        athat <- pretty(hii)
        axis(1, at = athat/(1 - athat), labels = paste(athat))
        if (one.fig) 
            title(sub = sub.caption, ...)
        p <- length(coef(x))
        bval <- pretty(sqrt(p * cook/g), 5)
        usr <- par("usr")
        xmax <- usr[2L]
        ymax <- usr[4L]
        for (i in seq_along(bval)) {
            bi2 <- bval[i]^2
            if (ymax > bi2 * xmax) {
                xi <- xmax + strwidth(" ")/3
                yi <- bi2 * xi
                abline(0, bi2, lty = 2)
                text(xi, yi, paste(bval[i]), adj = 0, xpd = TRUE)
            }
            else {
                yi <- ymax - 1.5 * strheight(" ")
                xi <- yi/bi2
                lines(c(0, xi), c(0, yi), lty = 2)
                text(xi, ymax - 0.8 * strheight(" "), paste(bval[i]), 
                  adj = 0.5, xpd = TRUE)
            }
        }
        mtext(getCaption(6), 3, 0.25, cex = cex.caption)
        if (id.n > 0) {
            show.r <- order(-cook)[iid]
            text.id(g[show.r], cook[show.r], show.r)
        }
        dev.flush()
    }
    if (!one.fig && par("oma")[3L] >= 1) 
        mtext(sub.caption, outer = TRUE, cex = 1.25)
    invisible()
}

Fig.9.8 <- function(){
par(mfrow = c(2, 2), mar = c(4,4,2,1.5)) 
plot(Fbird.lm, col.smooth = "gray")  
mtext("(a)",3,outer = TRUE, at = .05, line = -1.5, cex = 1.4)
mtext("(b)",3,outer = TRUE, at = .55, line = -1.5, cex = 1.4)
mtext("(c)",3,outer = TRUE, at = .05, line = -22, cex = 1.4)
mtext("(d)",3,outer = TRUE, at = .55, line = -22, cex = 1.4)
}

#############################################################################################

Fig.9.9 <- function(){ 
data(wash.rich)
X <- with(wash.rich, cbind(X1, X3, X4, X5))
Y <- wash.rich[,2]
l <- lm(Y~X)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 1, 1)) 
partial.resid.plot(l, lf.col = 1, sm.col = "gray50")

mtext("(a)",3,outer = TRUE, at = .05, line = -1.5, cex = 1.4)
mtext("(b)",3,outer = TRUE, at = .55, line = -1.5, cex = 1.4)
mtext("(c)",3,outer = TRUE, at = .05, line = -22, cex = 1.4)
mtext("(d)",3,outer = TRUE, at = .55, line = -22, cex = 1.4)
}

#############################################################################################

Fig.9.10<-function(){
dev.new(height=3.5)
par(mfrow =c(1,3), mar = c(0, 0, 0, 0), oma = c(4, 0, 2, 0))

x<-seq(1,10,.25)
y <- jitter(log(x),amount=.1)
y1 <- jitter(y[y>0], amount = .1)
x1 <- x[y>0]
plot(x1,y1,xaxt="n",yaxt="n",xlab="",ylab="", bty = "n", cex=1.4, xlim=c(min(x,x1),max(x,x1)), ylim=c(min(y1,log(x)),max(y1,log(x))))
points(x, log(x), type="l",lwd=2)

mtext(side=1,expression(paste(italic(X), "' = ln(", italic(X),")  or  ",italic(X),"' = ", sqrt(italic(X)))),cex=1.1, bty = "n", line =2)
mtext(side=3, "(a)",cex=1.5)


x1 <- jitter(log(x),amount=.1)
y1 <- jitter(x[x1>0], amount = .1)
x1 <- x1[x1>0]               
plot(x1,y1,xaxt="n",yaxt="n",xlab="",ylab="", bty = "n", cex=1.4, xlim=c(min(x1,log(x)),max(x1,log(x))), ylim=c(min(y1,x),max(y1,x)))
points(log(x),x,type="l",lwd=2)

mtext(side=1,expression(paste(italic(X), "' = ", italic(X)^2,"   or   ", italic(X), "' = ", italic(e^X))),cex = 1.1, bty = "n", line = 2)
mtext(side=3, "(b)",cex=1.5)
segments(min(x1,log(x))-.1,min(y1,x),min(x1,log(x))-.1,max(y1,x),lwd=1.5)

x1 <- jitter(-log(x),amount=.1)
y1 <- jitter(x,amount =.1)
plot(x1,y1,xaxt="n",yaxt="n",xlab="",ylab="", bty = "n", cex=1.4, xlim=c(min(x1,-log(x)),max(x1,-log(x))), ylim=c(min(y1,x),max(y1,x)))
points(-log(x),x, type="l", lwd=2)


mtext(side=1,expression(paste(italic(X), "' = ", italic(over(1,X)), "   or   ", italic(X), "' = ", italic(e^-X))),cex = 1.1, bty="n", line = 2)
mtext(side=3, "(c)",cex=1.5)
segments(min(x1,-log(x))-.1,min(y1,x),min(x1,-log(x))-.1,max(y1,x),lwd=1.5)

}


######################################################################################

Fig.9.11<-function(){
dev.new(height=3.5)
par(mar = c(0, 0, 0, 0), mfrow = c(1,3),  oma = c(4, 0, 2, 0))

x<-seq(1,10,.3)
plot(jitter(x), jitter(log(x), amount= .1 * x),xaxt="n", yaxt="n", xlab="", ylab="", bty = "n", cex=1.4)
points(x, log(x), type="l", lwd=2)
mtext(side=1,expression(paste(italic(Y), "' = ", sqrt(italic(Y)))),cex=1.1, bty = "n", line =2)
mtext(side=3, "(a)",cex=1.5)

jx <- jitter(x)
jy <- jitter(exp(-.3 * x), amount = .02 * x)
plot(jx, jy,xaxt="n",yaxt="n",xlab="",ylab="", bty = "n", cex=1.4)
points(x, exp(-.3 * x), type="l", lwd =2)
mtext(side=1,expression(paste(italic(Y),"' = ", log[10], italic(Y))),cex=1.1, bty = "n", line =2)
mtext(side=3, "(b)",cex=1.5)
segments(min(jx,x)-.3,min(jy,exp(-.3 * x)),min(jx,x)-.3,max(jy,exp(-.3 * x)))

jx <- jitter(x)
jy <- jitter(0.75 * x, amount = .25 * x)
plot(jx, jy, xaxt="n", yaxt="n", xlab="", ylab="", bty = "n", cex = 1.4)
points(x,0.75 * x, lwd = 2, type = "l")
mtext(side = 1, expression(paste(italic(Y), "' = ", italic(over(1,Y)))), cex = 1.1, line = 2)
mtext(side=3, "(c)",cex=1.5)
segments(min(jx,x)-.3,min(jy,0.75 * x),min(jx,x)-.3,max(jy,0.75 * x))
mtext(side = 1, "or", outer = T, line = 2, at = 0.34, cex = 1.1)
mtext(side = 1, "or", outer = T, line = 2, at = 0.67, cex = 1.1)
}

######################################################################################

Fig.9.12 <- function(){
arrow <- c(100, 54, 26, 12, 4, 1)
rice <- c(1, 2, 3, 4, 5, 6)
dev.new(height=3.5)
par(mfrow = c(1, 2), mar = c(5,0,0,1), oma=c(0,4,2,0))
plot(rice, arrow, ylab = "", xlab = "Rice density (seedlings/beaker)", type = "o")
mtext(side=3, "(a)", at = 0.4, cex = 1.4, line = 0.6)
plot(1/sqrt(rice), arrow, ylab = "", xlab = expression("(Rice density)"^-0.5), type = "o", yaxt = "n")
axis(side =2, labels = F)
mtext(side=3, "(b)", at = 0.36, cex = 1.4,bty="n", line = 0.6)
mtext(side=2, line = 2.8, "Arrowhead root growth (% of control)", outer = TRUE, at =.65)
}
 
## Also see:
P<-c(0.4,0.6,1.1,0.9,1.3,1.5,2.2,2.3,2.6,2.7)
Pheight<-c(42.5,53.5,70.3,84.2,87.6,102.3,107.4,112.6,113,119)
par(mfrow=c(2,2),mar=c(5,4,1,0.5))
plot(P,Pheight,xlab="P Addition to Soil (g)", ylab="Plant Height (cm)")
lines(lowess(Pheight~P,f=.66),col="red")
mtext(side=3,at=-.3,"(a)", line=0,cex=.9)
plot(log(P),Pheight,xlab="ln(P Addition to Soil (g))", ylab="Plant Height (cm)")
lines(lowess(Pheight~log(P),f=.66),col="red")
mtext(side=3,at=-1.5,"(b)", line=0,cex=.9)

######################################################################################

Fig.9.13<-function(){
Age<-c(rep(0,5),rep(1,5),rep(2,5),rep(3,5),rep(4,5))
Pamine<-c(13.44,12.84,11.91,20.09,15.6,10.11,11.38,10.28,8.96,8.59,9.83,9,8.65,7.85, 8.88,7.94,6.01,5.14,6.9,6.77,4.86,5.1,5.67,5.75,6.23)
dev.new(height=3.5)
layout(matrix(c(1,1,1,2,2,3,3,3), 1, 8, byrow = TRUE))
par(mar=c(5,4.5,.5,.3),cex=.9)
plot(Age,Pamine,xlab="Age (Years)",ylab="Polyamine level in plasma")
mtext(side=3, at=0.03,line = -1.2, "(a)", outer = TRUE, cex = 1.3)
boxcox(Pamine~Age)
mtext(side=3, at=0.4,line = -1.2, "(b)", outer = TRUE, cex = 1.3)
plot(Age, 1/sqrt(Pamine), xlab= "Age (Years) ",ylab=expression("(Polyamine level) "^-0.5))
mtext(side=3, at=0.65,line = -1.2, "(c)", outer = TRUE, cex = 1.3)
}

#######################################################################################

# Fig. 9.14
data(SM.temp.moist)
cent.day <- with(SM.temp.moist, (julian.day - mean(julian.day)))
poly.lm <- with(SM.temp.moist, lm(Temp_C ~ cent.day + I(cent.day^2)))

xv <- seq(-70, 70, .1)
new = data.frame(cent.day = xv)
yv <- predict(poly.lm, newdata = new)
par(cex=1.2)
plot(cent.day, SM.temp.moist$Temp_C, xlab = "Centered Julian day", ylab = "Soil temperature (\u00B0C)", cex = .95)
lines(xv, yv)


#######################################################################################

Fig.9.15 <- function(){
data(ant.dew)
par(cex = 1.2)
pch.no <- with(ant.dew, ifelse(direction == "D", 19, 1))
with(ant.dew, plot(head.width, ant.mass, xlab = "Head width (mm)", ylab = "Ant mass (mg)", pch = pch.no))
A <- ant.dew[ant.dew[,3] == "A",]
D <- ant.dew[ant.dew[,3] == "D",]
lmA <- lm(log(ant.mass) ~ log(head.width), data = A)
lmD <- lm(log(ant.mass) ~ log(head.width), data = D)
xv <- seq(1.2, 1.9, .01)
yvA <- exp(coef(lmA)[1] + coef(lmA)[2] * log(xv))
yvD <- exp(coef(lmD)[1] + coef(lmD)[2] * log(xv)) 
lines(xv, yvA)
lines(xv, yvD, lty = 2)
legend("topleft", pch = c(1, 19), legend = c("Descending", "Ascending"))
anova(lm(log(ant.mass) ~ log(head.width) * direction, data = ant.dew))  
}

##########################################################################################

Fig.9.16<-function(){
X<-seq(0.01,1,.01)
epsilon<-matrix(ncol=10,nrow=100,data=rnorm(1000,0,0.1))
E.Y<-matrix(nrow=100,ncol=10,rep(exp((X-0.03)^2)-1,10))
Y<-E.Y+epsilon
dev.new(height=3.5, pointsize=11)
par(mfrow=c(1,2),mar=c(0,.2,0,.8),oma=c(4.5,3.8,1,0))
lm1<-lm(Y~X)$coefficients
plot(X,Y[,1],ylim=c(-0.2,0.8),xlim=c(0,1),type = "n", ylab="", xlab = "", cex.axis=.9)
lines(X,exp((X-0.03)^2)-1,lwd=2)
for(i in 1:10)abline(a=lm1[1,][i],b=lm1[2,][i],col="gray")
legend("bottomright",lty=1,col=c("black","gray"),lwd=c(2,1),legend=c(expression(paste(italic(Y)," = ",e^(italic(X)+0.3)^2, - 1)),legend=expression(paste(italic(E),"(",italic(Y),") = ",beta[0], " + ", beta[1],italic(X),))),bty="n",cex=.8)

plot(X,Y[,1],ylim=c(-0.2,0.8),xlim=c(0,1),type= "n",xlab="",ylab="",yaxt="n", cex.axis=.9)
lines(X,exp((X-0.03)^2)-1,lwd=2)
fitted.lm2<-matrix(ncol=10,nrow=100)
for(i in 1:10)fitted.lm2[,i]<-fitted(lm(Y[,i]~X+I(X^2)+I(X^3)+I(X^4)+ I(X^5)))
for(i in 1:10)points(X,fitted.lm2[,i],col="gray",type="l")
legend("bottomleft",lty=1,col=c("black","gray"),lwd=c(2,1),legend=c(expression(paste(italic(Y)," = ",e^(italic(X)+0.3)^2, - 1)),expression(paste(italic(E),"(",italic(Y),") = ",beta[0], " + ",beta[1],italic(X), " + ", beta[2],italic(X)^2, " + ", beta[3],italic(X)^3, " + ", beta[4],italic(X)^4, " + ", beta[5],italic(X)^5))),bty="n",cex=.8)
axis(side=2, labels=F, cex.axis = .9)
mtext(outer=T,side=2,expression(italic(Y)), line = 2.4)
mtext(outer=T,side=1,expression(italic(X)), line = 2.7)
}

Fig.9.17 <- function(){
data(Fbird)
Fbird1 <- rbind(Fbird, matrix(nrow = 2, ncol = 2,data = c(7700, 8200, 560, 555), dimnames = list(c(19, 20), c("vol", "freq"))))
Fbird.lm <- lm(freq ~ vol, data = Fbird)
Fbird1.lm <- lm(freq ~ vol, data = Fbird1)
library(MASS)
M <- coef(rlm(freq ~ vol, data = Fbird1))
S <- coef(lqs(freq ~ vol, data = Fbird1, method = "S"))
MM <- coef(rlm(freq ~ vol, data = Fbird1, method = "MM"))
par(cex = 1.2, bg = "white", mar = c(5,4.5,1,1))
plot(Fbird1[,1],Fbird1[,2], xlab = expression(paste("Estimated gular pouch size ( ",cm^3,")")), ylab = "Fundamental frequency (Hz)", pch = c(rep(1, 18), rep(19, 2))) 
a <- c(coef(Fbird.lm)[1], coef(Fbird1.lm)[1], M[1], S[1], MM[1]) 
b <- c(coef(Fbird.lm)[2], coef(Fbird1.lm)[2], M[2], S[2], MM[2])
for(i in 1:5) abline(a[i], b[i], lty = i , col = gray(i/6), lwd = 2) 
legend("bottomleft", bty="n", lty= 1:5, col = gray((1:5)/6), lwd = 2,  legend = c("OLS; no outliers", "OLS; with outliers", expression(italic(M)), expression(italic(S)), expression(italic(MM))))
}

################################################################################


Fig.9.18<-function(){
par(cex = 1.2, mar=c(5,4.3,1.5,1.5))
plot(seq(2,7),seq(2,7),xlab=expression(italic(X)),ylab=expression(italic(Y)),type="n",xaxt="n",yaxt="n")
X1<-seq(4,5,.01)
X2<-seq(5.01,6,.01)
Y1<--X1+10
Y2<-X2
X<-c(X1,X2)
Y<-c(Y1,Y2)

polygon(X,Y,col="gray",border=NA)
abline(0,1,lwd=1.5)
points(4,6)
segments(4,4,4,6,lwd=1.5)
segments(5,5,4,6,lty=2,lwd=1.5)
legend("topleft",lty=c(1,2,1),lwd=c(1.25,1.25,10),col=c("black","black","gray"),legend=c("OLS","MA","RMA"))
}

################################################################################

Fig.9.19 <- function(lwd=1.5){
install.packages("lmodel2")
library(lmodel2)
data(mod2ex2)
par(cex = 1.2, bg = "white", mar = c(5,4.5,1,1))
with(mod2ex2, plot(Predators, Prey, ylab = expression(paste("Number of  ", italic("Macomona lilana"), " inds.")), xlab = expression(paste("Number of  ", italic("Myliobatis tenuicaudatus")," inds."))))
abline(20.0267, 2.6315, lty = 1, col = gray(0), lwd=lwd)
abline(13.05968, 3.465907, lty = 2, col = gray(.2),lwd=lwd)
abline(16.45205, 3.059635, lty = 3, col = gray(.4),lwd=lwd)
abline(17.25651, 2.963292, lty = 4, col = gray(.6),lwd=lwd)
legend("topleft", lty = seq(1,4),col = gray(c(0, .2, .4, .6)), legend = c("OLS", "MA", "SMA", "RMA"), bty="n", lwd=lwd)
}

################################################################################

Fig.9.20 <- function(){
data(beetle)
beetle.glm <- glm(ANAT ~ Wood.density,  family = binomial(link = "logit"), data = beetle)
dev.new(height=3.5)
par(mfrow = c(1, 2), mar = c(0,4.2,1.8,1), oma=c(4.3,0,0,0))
o <- with(beetle,order(Wood.density))
with(beetle, plot(Wood.density, ANAT, xlab="", ylab = expression(paste(italic("P"),"(", italic("A. attenuatus"), " presence)")),cex.lab=.9,cex.axis=.9))
with(beetle, points(Wood.density[o], fitted(beetle.glm)[o], type = "l"))

co <- coef(beetle.glm)
with(beetle, plot(Wood.density, co[1]+(Wood.density*co[2]), xlab = "", ylab = expression(paste("Logit(", italic("A. attenuatus"), " presence)")), type = "l",cex.lab=.9,cex.axis=.9))
#with(beetle,rug(Wood.density))
mtext("(a)", cex = 1.4, side = 3, at = 0.03, outer = T, line =-1.3)
mtext("(b)", cex = 1.4, side = 3, at = 0.53, outer = T, line = -1.3)
mtext(side=1, expression(paste("Wood density ", "g ", cm^-3)), outer=T, line = 3, cex=.9)
}
 
#################################################################################

#################################################################################

Fig.9.22 <- function(){
data(crabs)
crab.glm <- glm(satell ~ width,  family = poisson(link = "log"), data = crabs)
dev.new(height=3.5)
par(mfrow = c(1, 2), mar = c(0,4.2,2,1), oma=c(4.5,0,0,0))
o <- with(crabs, order(width))
with(crabs, plot(width, satell, xlab = "", ylab = "Satellite count", cex=.9, cex.lab=.9, cex.axis=.9))
with(crabs, points(width[o], fitted(crab.glm)[o], type = "l", cex=.5))
co <- coef(crab.glm)
with(crabs, plot(width, co[1]+(co[2]*width), xlab = "", ylab = expression(paste(log[e], "(satellite count)")), type = "l", cex=.9, cex.axis=.9, cex.lab=.9))
with(crabs, points(width, log(satell), cex=.9))
mtext("(a)", cex = 1.4, side = 3, at = 0.03, outer = T, line =-1.3)
mtext("(b)", cex = 1.4, side = 3, at = 0.53, outer = T, line = -1.3)
mtext(side=1, "Carapace width (cm)", outer=T, line = 3, cex=.9)
}


#################################################################################


Fig.9.23 <- function(){
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1.5)) 
plot(beetle.glm, col.smooth = "gray")  
mtext("(a)",3,outer = TRUE, at = .05, line = -1.5, cex = 1.3)
mtext("(b)",3,outer = TRUE, at = .55, line = -1.5, cex = 1.3)
mtext("(c)",3,outer = TRUE, at = .05, line = -22, cex = 1.3)
mtext("(d)",3,outer = TRUE, at = .55, line = -22, cex = 1.3)
}


# Fig 9.26
par(cex=1.2, mar=c(5,4.5,2,2))
plot(beta.f.x, xlab=expression(paste(beta[0], "|",italic(Y), sigma^2)),ylab =  expression(paste(beta[1], "|",italic(Y), sigma^2)))


