########################################
########################################
# fit exogenous factors with backfitting
########################################
########################################
xogen = function(t,x,volc,nino,ssn,nback=4,
    t.2fit=c(1950,2025),method="lag"){
####
    frac = 20/diff(range(t))    # fraction for lowess call
    ##########
    # 1st pass
    ##########
    g = lowess(t,x,f=frac)
    y = x-g$y
    zback = fit.xogen(t,x,y,volc,nino,ssn,t.2fit=t.2fit,method=method)
    #############
    # backfitting
    #############
    if (nback >= 1){
        for (jback in 1:nback){
            z = zback$zout$adj
            g = lowess(t,z,f=frac)
            y = x-g$y
            zback = fit.xogen(t,x,y,volc,nino,ssn,t.2fit=t.2fit,method=method)
        }
    }
    zback
}

######################
######################
# fit xogenous factors
# to "y"
# exp.smoothed
# volc, nino, ssn
######################
######################
fit.xogen = function(t,x,y,volc,nino,ssn,t.2fit=c(1950,2025),method="lag"){
    n.2fit = which(t >= t.2fit[1] & t < t.2fit[2])
    #########################
    # fit "y" to
    # exp.smoothed predictors
    #########################
    best.d1 = -1
    best.d2 = -1
    best.d3 = -1
    best.aic = 999999

    for (d1 in 0:24){
        f1 = simplag(volc,d1,method=method)
        for (d2 in 0:12){
            f2 = simplag(nino,d2,method=method)
            for (d3 in 0:12){
                f3 = simplag(ssn,d3,method=method)
                z2fit = data.frame(y,volc=f1,nino=f2,ssn=f3)
                z2fit = z2fit[n.2fit,]
                q = lm(z2fit)
                this.aic = AIC(q)
                if (this.aic < best.aic){
                    best.d1 = d1
                    best.d2 = d2
                    best.d3 = d3
                    best.aic = this.aic
                }
            }
            plot(0,0,main=paste(d1,d2,d3))
        }
    }
    best.d = c(best.d1,best.d2,best.d3); best.d

    f1 = simplag(volc,best.d1,method=method)
    f2 = simplag(nino,best.d2,method=method)
    f3 = simplag(ssn,best.d3,method=method)
    z2fit = data.frame(y,volc=f1,nino=f2,ssn=f3)
    z2fit = z2fit[n.2fit,]
    yfit = lm(z2fit)

    #################
    # adjusted values
    # this pass
    #################
    cc = yfit$coef
    y = cc[1]+cc[2]*f1+cc[3]*f2+cc[4]*f3
    z = x-y

    zout = data.frame(t,raw=x,adj=z,
        fit=y,
        volc=cc[2]*f1,nino=cc[3]*f2,ssn=cc[4]*f3)
    zout = round(zout,4)
    zback = list(zout=zout,best.d=best.d,yfit=yfit)
    invisible(zback)
}

###############
# simple lag
# or exp.smooth
###############
simplag = function(x,t.const=NULL,method="lag"){
    if (!is.finite(t.const)){
        stop("must specify time constant (t.const)")
    }
    ntot = length(x)
    if (method == "exp"){
        if (t.const == 0){
                lam = 0
        } else{
            lam = exp(-1/t.const)
        }
        y = numeric(ntot)
        y[1] = (1-lam)*x[1]
        for (j in 2:ntot){
            y[j] = lam*y[j-1] + (1-lam)*x[j]
        }
    } else{
        ya = rep(x[1],t.const)
        yb = x[1:(ntot-t.const)]
        y = c(ya,yb)
    }
    y
}
