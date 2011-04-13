# modification of the blowfly code in pomp, to allow for more general dt
# here, set up for dt=1 and dt=2
# dt is hard-coded, and initial values are customized for each dt

require(pomp)

TAU.BIDAY <- 7
TAU.DAY <- 2*TAU.BIDAY
  # delay, in days, treated as fixed following xia and tong
  # xia and tong claim to be using tau=8 bidays, but on inspection 
  # their Euler method is really tau=7 bidays

raw.data <- subset(read.csv2("blowfly.csv",comment.char="#"),set==4)
y <- raw.data[(TAU.BIDAY+2):200,c("day","y")]
y$day <- y$day - 2*TAU.BIDAY  
# here, we measure time in days after observation number TAU.BIDAY.
# this simplifies initial conditions, and produces likelihoods
# compatible with xia and tong


if(0){ # hard coded in the function; initial conditions for dt=1
 y.init <- raw.data[1:(TAU.BIDAY+1),2]
 t.init <- raw.data[1:(TAU.BIDAY+1),1]
 t.out <- 0:max(t.init)
 y.init.smo <- spline(x=t.init,y=y.init,xout=t.out)
 Y.INIT <- y.init.smo$y
}

if(0){ # hard coded in the function; initial conditions for dt=2
 Y.INIT <- raw.data[1:(TAU+1),2]
}


pomp(
     data=y,
     times="day",
     t0=0,
     rprocess=euler.sim(
       step.fun="_blowfly_model_simulator",
       delta.t=1,
       ),
     paramnames=c("log.P","log.N0","log.delta","log.sigma.P","log.sigma.d","tau","log.sigma.y"),
     statenames=c("N1","R","S","e","eps"),
     obsnames=c("y"),
     measurement.model=y~nbinom(mu=N1,size=exp(-2*log.sigma.y)),
     initializer=function (params, t0, ...) {
       y.init <- c(948, 948, 942, 930, 911, 885, 858, 833.7, 801, 748.3, 676, 589.8, 504, 434.9, 397) 
#       y.init <- c(948, 942, 911, 858, 801, 676, 504, 397)
       ntau <- length(y.init)
       n <- y.init[ntau:1]
       names(n) <- paste("N",seq_len(ntau),sep="")
       c(n,R=0,S=0,e=0,eps=0)
     }
     ) -> bf.dt1

pomp(
     data=y,
     times="day",
     t0=0,
     rprocess=euler.sim(
       step.fun="_blowfly_model_simulator",
       delta.t=2,
       ),
     paramnames=c("log.P","log.N0","log.delta","log.sigma.P","log.sigma.d","tau","log.sigma.y"),
     statenames=c("N1","R","S","e","eps"),
     obsnames=c("y"),
     measurement.model=y~nbinom(mu=N1,size=exp(-2*log.sigma.y)),
     initializer=function (params, t0, ...) {
#      y.init <- c(948, 948, 942, 930, 911, 885, 858, 833.7, 801, 748.3, 676, 589.8, 504, 434.9, 397) 
       y.init <- c(948, 942, 911, 858, 801, 676, 504, 397)
       ntau <- length(y.init)
       n <- y.init[ntau:1]
       names(n) <- paste("N",seq_len(ntau),sep="")
       c(n,R=0,S=0,e=0,eps=0)
     }
     ) -> bf.dt2

params.best.dt1 <- c(  # mle from search to date
log.P = 1.189 , 
 log.delta = -1.828 , 
 log.N0 = 6.522 , 
 log.sigma.P = 0.301 , 
 log.sigma.d = -0.292 , 
 log.sigma.y = -3.625 , 
 tau = 14 
)

params.best.dt2 <- c(  # mle from search to date
log.P = 1.005 , 
 log.delta = -1.75 , 
 log.N0 = 6.685 , 
 log.sigma.P = 0.366 , 
 log.sigma.d = -0.274 , 
 log.sigma.y = -4.524 , 
 tau = 7 
)

test <- FALSE
if(test){
 sim.dt1 <- simulate(bf.dt1,params=params.best.dt1,nsim=1)
 plot(data.array(sim.dt1)['y',],ty='l')
 lines(data.array(bf.dt1)['y',],lty="dashed")
 states(sim.dt1)[,1]

 sim.dt2 <- simulate(bf.dt2,params=params.best.dt2,nsim=1)
 plot(data.array(sim.dt2)['y',],ty='l')
 lines(data.array(bf.dt2)['y',],lty="dashed")
 states(sim.dt2)[,1]

 # check that it matches the deterministic skeleton when noise is small
 params.1.skel <- params.best.dt1
 params.1.skel["log.sigma.P"] <- log(0.00001)
 params.1.skel["log.sigma.d"] <- log(0.00001)
 params.1.skel["log.sigma.y"] <- log(0.00001)
 simulate(bf.dt1,params=params.1.skel,nsim=1, seed=73691676L) -> b1.skel

 plot(data.array(b1.skel)['y',],ty='l')
 lines(data.array(bf.dt1)['y',],lty="dashed") 
 
} 

