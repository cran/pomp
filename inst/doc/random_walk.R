###################################################
### chunk number 1: 
###################################################
  options(keep.source=TRUE,width=60)
  library(pomp)


###################################################
### chunk number 2: 
###################################################
  rw.rprocess <- function (xstart, times, params, ...) { 
    ## this function simulates two independent random walks with intensities s1, s2
    nsims <- ncol(params)
    ntimes <- length(times)
    dt <- diff(times)
    x <- array(0,dim=c(2,nsims,ntimes))
    rownames(x) <- rownames(xstart)
    noise.sds <- params[c('s1','s2'),]
    x[,,1] <- xstart
    for (j in 2:ntimes) {
      ## we are mimicking a continuous-time process, so the increments have SD ~ sqrt(dt)
      ## note that we do not have to assume that 'times' are equally spaced
      x[,,j] <- rnorm(n=2*nsims,mean=x[,,j-1],sd=noise.sds*dt[j-1])
    }
    x
  }


###################################################
### chunk number 3: 
###################################################
  rw.dprocess <- function (x, times, params, log = FALSE, ...) { 
    ## given a sequence of consecutive states in 'x', this function computes the p.d.f.
    nsims <- ncol(params)
    ntimes <- length(times)
    dt <- diff(times)
    d <- array(0,dim=c(2,nsims,ntimes-1))
    noise.sds <- params[c('s1','s2'),]
    for (j in 2:ntimes)
      d[,,j-1] <- dnorm(x[,,j]-x[,,j-1],mean=0,sd=noise.sds*dt[j-1],log=TRUE)
    if (log) {
      apply(d,c(2,3),sum)
    } else {
      exp(apply(d,c(2,3),sum))
    }
  }


###################################################
### chunk number 4: 
###################################################
  bvnorm.rmeasure <- function (x, times, params, ...) {
    ## noisy observations of the two walks with common noise SD 'tau'
    nsims <- dim(x)[2]
    ntimes <- dim(x)[3]
    y <- array(0,dim=c(2,nsims,ntimes))
    rownames(y) <- c('y1','y2')
    for (k in 1:nsims) {
      for (j in 1:ntimes) {
	y[,k,j] <- rnorm(2,mean=x[,k,j],sd=params['tau',k])
      }
    }
    y
  }


###################################################
### chunk number 5: 
###################################################
  bvnorm.dmeasure <- function (y, x, times, params, log = FALSE, ...) {
    ## noisy observations of the two walks with common noise SD 'tau'
    d1 <- dnorm(
                x=y['y1',],
                mean=x['x1',,],
                sd=params['tau',],
                log=TRUE
                )
    d2 <- dnorm(
                x=y['y2',],
                mean=x['x2',,],
                sd=params['tau',],
                log=TRUE
                )
    if (log) {
      sum(d1,d2,na.rm=T)
    } else {
      exp(sum(d1,d2,na.rm=T))
    }
  }


###################################################
### chunk number 6: 
###################################################
rw2 <- pomp(
            rprocess = rw.rprocess,
            dprocess = rw.dprocess,
            rmeasure = bvnorm.rmeasure,
            dmeasure = bvnorm.dmeasure,
            times=1:100,
            data=rbind(
              y1=rep(0,100),
              y2=rep(0,100)
              ),
            t0=0,
            useless=23
            )


###################################################
### chunk number 7: 
###################################################
p <- rbind(s1=c(2,2,3),s2=c(0.1,1,2),tau=c(1,5,0))
x0 <- rbind(x1=c(0,0,5),x2=c(0,0,0))


###################################################
### chunk number 8: 
###################################################
examples <- simulate(rw2,xstart=x0,params=p)
rw2 <- examples[[1]]


###################################################
### chunk number 9: 
###################################################
y <- simulate(rw2,xstart=x0,params=p,obs=T,states=T)
y <- simulate(rw2,xstart=x0,params=p,obs=T)
x <- simulate(rw2,xstart=x0,params=p,states=T)
x <- simulate(rw2,nsim=10,xstart=x0,params=p,states=T)
x <- simulate(rw2,nsim=10,xstart=x0[,1],params=p[,1],states=T)
x <- simulate(rw2,nsim=10,xstart=x0[,1],params=p[,1],obs=T,states=T)
x <- simulate(rw2,nsim=10,xstart=x0,params=p[,1],obs=T,states=T)
x <- simulate(rw2,nsim=10,xstart=x0[,2],params=p,obs=T,states=T)
x <- simulate(rw2,nsim=10,xstart=x0[,1],params=p[,1])


###################################################
### chunk number 10: 
###################################################
plot(rw2)


###################################################
### chunk number 11: 
###################################################
x <- data.array(rw2)
t <- time(rw2)


###################################################
### chunk number 12: 
###################################################
x <- rprocess(rw2,xstart=x0,times=0:100,params=p)


###################################################
### chunk number 13: 
###################################################
y <- rmeasure(rw2,x=x[,,-1,drop=F],times=1:100,params=p)


###################################################
### chunk number 14: 
###################################################
dprocess(rw2,x[,,6:11],times=5:10,params=p)
dprocess(rw2,x[,,6:11],times=5:10,params=p,log=T)


###################################################
### chunk number 15: 
###################################################
dmeasure(rw2,y=y[,1,1:4],x=x[,,2:5,drop=F],times=time(rw2)[1:4],p)
dmeasure(rw2,y=y[,2,1:4],x=x[,,2:5,drop=F],times=time(rw2)[1:4],p)
dmeasure(rw2,y=y[,3,1:4],x=x[,,2:5,drop=F],times=time(rw2)[1:4],p)
exp(dmeasure(rw2,y=y[,3,1:4],x=x[,,2:5,drop=F],times=time(rw2)[1:4],p,log=T))


###################################################
### chunk number 16: 
###################################################
  ou2.rprocess <- function (xstart, times, params, ...) { 
    ## this function simulates two discrete-time OU processes
    nsims <- ncol(xstart)
    ntimes <- length(times)
    alpha <- array(params[c('alpha.1','alpha.2','alpha.3','alpha.4'),],dim=c(2,2,nsims))
    sigma <- array(params[c('sigma.1','sigma.2','sigma.2','sigma.3'),],dim=c(2,2,nsims))
    sigma[1,2,] <- 0
    x <- array(0,dim=c(2,nsims,ntimes))
    rownames(x) <- rownames(xstart)
    x[,,1] <- xstart
    for (k in 1:nsims) {
      for (j in 2:ntimes) {
	x[,k,j] <- alpha[,,k]%*%x[,k,j-1]+sigma[,,k]%*%rnorm(2)
      }
    }
    x
  }


###################################################
### chunk number 17: 
###################################################
  ou2.dprocess <- function (x, times, params, log = FALSE, ...) { 
    ## this function simulates two discrete-time OU processes
    nsims <- ncol(x)
    ntimes <- length(times)
    alpha <- array(params[c('alpha.1','alpha.2','alpha.3','alpha.4'),],dim=c(2,2,nsims))
    sigma <- array(params[c('sigma.1','sigma.2','sigma.2','sigma.3'),],dim=c(2,2,nsims))
    sigma[1,2,] <- 0
    d <- array(0,dim=c(nsims,ntimes-1))
    for (k in 1:nsims) {
      for (j in 2:ntimes) {
        z <- forwardsolve(sigma[,,k],x[,k,j]-alpha[,,k]%*%x[,k,j-1])
        if (log) {
	  d[k,j-1] <- sum(dnorm(z,mean=0,sd=1,log=TRUE))
	} else {
	  d[k,j-1] <- exp(sum(dnorm(z,mean=0,sd=1,log=TRUE)))
	}
      }
    }
    d
  }


###################################################
### chunk number 18: 
###################################################
ou2 <- pomp( 
	    times=seq(1,100),
	    data=rbind(
	      y1=rep(0,100),
	      y2=rep(0,100)
	      ),
	    t0=0,
	    rprocess = ou2.rprocess,
	    dprocess = ou2.dprocess,
	    rmeasure = bvnorm.rmeasure,
	    dmeasure = bvnorm.dmeasure
	    )


###################################################
### chunk number 19: 
###################################################
x0 <- c(x1=50,x2=-50)


###################################################
### chunk number 20: 
###################################################
p <- c(
       alpha.1=0.9,alpha.2=0,alpha.3=0,alpha.4=0.99,
       sigma.1=1,sigma.2=0,sigma.3=2,
       tau=1
       )


###################################################
### chunk number 21: 
###################################################
 tic <- Sys.time()
 ou2 <- simulate(ou2,xstart=x0,params=p,nsim=1000)[[1]]
 toc <- Sys.time()
 print(toc-tic)


###################################################
### chunk number 22: 
###################################################
x <- rprocess(ou2,xstart=as.matrix(x0),times=c(0,time(ou2)),params=as.matrix(p))


###################################################
### chunk number 23: 
###################################################
y <- rmeasure(ou2,x=x[,,-1,drop=F],times=time(ou2),params=as.matrix(p))


###################################################
### chunk number 24: 
###################################################
dprocess(ou2,x[,,36:41,drop=F],times=time(ou2)[35:40],params=as.matrix(p))
exp(dprocess(ou2,x[,,36:41,drop=F],times=time(ou2)[35:40],params=as.matrix(p),log=T))


###################################################
### chunk number 25: 
###################################################
dmeasure(ou2,y=y[,1,1:4],x=x[,,2:5,drop=F],times=time(ou2)[1:4],params=as.matrix(p))
exp(dmeasure(ou2,y=y[,1,1:4],x=x[,,2:5,drop=F],times=time(ou2)[1:4],params=as.matrix(p),log=T))


###################################################
### chunk number 26: 
###################################################
  ou2.rprocess <- function (xstart, times, params, ...) 
    .Call('ou2_simulator',xstart,times,params)


###################################################
### chunk number 27: 
###################################################
  ou2.dprocess <- function (x, times, params, log = FALSE, ...)
    .Call('ou2_density',x,as.numeric(times),params,log)


###################################################
### chunk number 28: 
###################################################
  bvnorm.dmeasure <- function (y, x, times, params, log = FALSE, ...)
    .Call('bivariate_normal_dmeasure',y,x,as.numeric(times),params,log)


###################################################
### chunk number 29: 
###################################################
  bvnorm.rmeasure <- function (x, times, params, ...)
    .Call('bivariate_normal_rmeasure',x,as.numeric(times),params)


###################################################
### chunk number 30: 
###################################################
ou2 <- pomp( 
	    times=seq(1,100),
	    data=rbind(
	      y1=rep(0,100),
	      y2=rep(0,100)
	      ),
	    t0=0,
	    rprocess = ou2.rprocess,
	    dprocess = ou2.dprocess,
	    rmeasure = bvnorm.rmeasure,
	    dmeasure = bvnorm.dmeasure,
            ivpnames = c('x1.0','x2.0'),
            parnames = c(
              'alpha.1','alpha.2','alpha.3','alpha.4',
              'sigma.1','sigma.2','sigma.3',
              'tau')
	    )
 tic <- Sys.time()
 ou2 <- simulate(ou2,xstart=x0,params=p,nsim=1000)[[1]]
 toc <- Sys.time()
 print(toc-tic)


###################################################
### chunk number 31: 
###################################################
  plot(ou2)


###################################################
### chunk number 32: 
###################################################
x <- rprocess(ou2,xstart=as.matrix(x0),times=c(0,time(ou2)),params=as.matrix(p))


###################################################
### chunk number 33: 
###################################################
y <- rmeasure(ou2,x=x[,,-1,drop=F],times=time(ou2),params=as.matrix(p))


###################################################
### chunk number 34: 
###################################################
log(dprocess(ou2,x[,,36:41,drop=F],times=time(ou2)[35:40],params=as.matrix(p)))
dprocess(ou2,x[,,36:41,drop=F],times=time(ou2)[35:40],params=as.matrix(p),log=T)


###################################################
### chunk number 35: 
###################################################
log(dmeasure(ou2,y=y[,1,1:4],x=x[,,2:5,drop=F],times=time(ou2)[1:4],params=as.matrix(p)))
dmeasure(ou2,y=y[,1,1:4],x=x[,,2:5,drop=F],times=time(ou2)[1:4],params=as.matrix(p),log=T)


###################################################
### chunk number 36: 
###################################################
save(list='ou2',file='ou2.rda')


###################################################
### chunk number 37: 
###################################################
X0 <- matrix(x0,2,2000)
rownames(X0) <- c('x1','x2')
fit1 <- pfilter(ou2,X0,p,filter.mean=T,pred.mean=T,pred.var=T)


###################################################
### chunk number 38: 
###################################################
kalman.filter <- function (y, x0, a, b, sigma, tau) {
  n <- nrow(y)
  ntimes <- ncol(y)
  sigma.sq <- sigma%*%t(sigma)
  tau.sq <- tau%*%t(tau)
  inv.tau.sq <- solve(tau.sq)
  cond.dev <- numeric(ntimes)
  filter.mean <- matrix(0,n,ntimes)
  pred.mean <- matrix(0,n,ntimes)
  pred.var <- array(0,dim=c(n,n,ntimes))
  dev <- 0
  m <- x0
  v <- diag(0,n)
  for (k in seq(length=ntimes)) {
    pred.mean[,k] <- M <- a%*%m
    pred.var[,,k] <- V <- a%*%v%*%t(a)+sigma.sq
    q <- b%*%V%*%t(b)+tau.sq
    r <- y[,k]-b%*%M
    cond.dev[k] <- n*log(2*pi)+log(det(q))+t(r)%*%solve(q,r)
    dev <- dev+cond.dev[k]
    q <- t(b)%*%inv.tau.sq%*%b+solve(V)
    v <- solve(q)
    filter.mean[,k] <- m <- v%*%(t(b)%*%inv.tau.sq%*%y[,k]+solve(V,M))
  }
  list(
       pred.mean=pred.mean,
       pred.var=pred.var,
       filter.mean=filter.mean,
       cond.loglik=-0.5*cond.dev,
       loglik=-0.5*dev
       )
}


###################################################
### chunk number 39: 
###################################################
y <- data.array(ou2)
a <- matrix(p[c('alpha.1','alpha.2','alpha.3','alpha.4')],2,2)
b <- diag(1,2)
sigma <- matrix(c(p['sigma.1'],p['sigma.2'],0,p['sigma.3']),2,2)
tau <- diag(p['tau'],2,2)
fit2 <- kalman.filter(y,x0,a,b,sigma,tau)


###################################################
### chunk number 40: 
###################################################
normal.particles <- function (Np, center, sd, ivpnames, ...) {
  params <- matrix(
                   rnorm(Np*length(center),mean=center,sd=sd),
                   length(center),Np,
                   dimnames=list(names(center),NULL)
                   )
  states <- params[ivpnames,,drop=FALSE]
  rownames(states) <- gsub('.0','',ivpnames)
  list(
       states=states,
       params=params
       )
}


###################################################
### chunk number 41: 
###################################################
  ou2 <- mif(ou2,Nmif=0,start=c(x1.0=50,x2.0=-50,p),
             pars=c('alpha.1','alpha.4'),ivps=c('x1.0','x2.0'),
             particles=normal.particles,
             rw.sd=c(
               x1.0=5,x2.0=5,
               alpha.1=0.1,alpha.2=0,alpha.3=0,alpha.4=0.1,
               sigma.1=0,sigma.2=0,sigma.3=0,
               tau=0
               ),
             alg.pars=list(Np=1000,var.factor=1,ic.lag=10,cooling.factor=0.95),
             max.fail=100
             )


###################################################
### chunk number 42: 
###################################################
coef(ou2,c('x1.0','x2.0','alpha.1','alpha.4')) <- c(45,-60,0.8,0.9)
tic <- Sys.time()
fit <- mif(ou2,Nmif=2,max.fail=100)
fit <- continue(fit,Nmif=78,max.fail=100)
toc <- Sys.time()
print(toc-tic)
coef(fit)


###################################################
### chunk number 43: 
###################################################
pfilter(fit)$loglik


###################################################
### chunk number 44: 
###################################################
par(mfrow=c(3,1))
plot(conv.rec(fit,'loglik'),type='l')
plot(conv.rec(fit,'alpha.1'),type='l')
plot(conv.rec(fit,'alpha.4'),type='l')
par(mfrow=c(1,1))


###################################################
### chunk number 45: 
###################################################
plot(simulate(fit)[[1]])


