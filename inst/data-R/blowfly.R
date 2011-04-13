require(pomp)

pomp(
     data=read.csv("blowfly1.csv",comment.char="#"),
     times="day",
     t0=0,
     rprocess=euler.sim(
       step.fun="_blowfly_simulator",
       delta.t=1
       ),
     paramnames=c("log.P","log.N0","log.delta","log.sigma.P","log.sigma.d","tau","log.sigma.y"),
     statenames=c("N1","R","S","e","eps"),
     obsnames=c("y"),
     measurement.model=y~norm(mean=N1,sd=exp(log.sigma.y)),
     initializer=function (params, t0, ...) {
       ntau <- as.integer(params["tau"])+1
       ninit <- round(exp(params["log.N0"]))
       n <- rep(ninit,times=ntau)
       names(n) <- paste("N",seq_len(ntau),sep="")
       c(n,R=0,S=0,e=0,eps=0)
     }
     ) -> blowfly1

pomp(
     data=read.csv("blowfly2.csv",comment.char="#"),
     times="day",
     t0=200,
     rprocess=euler.sim(
       step.fun="_blowfly_simulator",
       delta.t=1
       ),
     paramnames=c("log.P","log.N0","log.delta","log.sigma.P","log.sigma.d","tau","log.sigma.y"),
     statenames=c("N1","R","S","e","eps"),
     obsnames=c("y"),
     measurement.model=y~norm(mean=N1,sd=exp(log.sigma.y)),
     initializer=function (params, t0, ...) {
       ntau <- as.integer(params["tau"])+1
       ninit <- round(exp(params["log.N0"]))
       n <- rep(ninit,times=ntau)
       names(n) <- paste("N",seq_len(ntau),sep="")
       c(n,R=0,S=0,e=0,eps=0)
     }
     ) -> blowfly2

pomp(
     data=read.csv("blowfly3.csv",comment.char="#"),
     times="day",
     t0=200,
     rprocess=euler.sim(
       step.fun="_blowfly_simulator",
       delta.t=1
       ),
     paramnames=c("log.P","log.N0","log.delta","log.sigma.P","log.sigma.d","tau","log.sigma.y"),
     statenames=c("N1","R","S","e","eps"),
     obsnames=c("y"),
     measurement.model=y~norm(mean=N1,sd=exp(log.sigma.y)),
     initializer=function (params, t0, ...) {
       ntau <- as.integer(params["tau"])+1
       ninit <- round(exp(params["log.N0"]))
       n <- rep(ninit,times=ntau)
       names(n) <- paste("N",seq_len(ntau),sep="")
       c(n,R=0,S=0,e=0,eps=0)
     }
     ) -> blowfly3

pomp(
     data=read.csv("blowfly4.csv",comment.char="#"),
     times="day",
     t0=0,
     rprocess=euler.sim(
       step.fun="_blowfly_simulator",
       delta.t=1
       ),
     paramnames=c("log.P","log.N0","log.delta","log.sigma.P","log.sigma.d","tau","log.sigma.y"),
     statenames=c("N1","R","S","e","eps"),
     obsnames=c("y"),
     measurement.model=y~norm(mean=N1,sd=exp(log.sigma.y)),
     initializer=function (params, t0, ...) {
       ntau <- as.integer(params["tau"])+1
       ninit <- round(exp(params["log.N0"]))
       n <- rep(ninit,times=ntau)
       names(n) <- paste("N",seq_len(ntau),sep="")
       c(n,R=0,S=0,e=0,eps=0)
     }
     ) -> blowfly4

simulate(
         pomp(
              data=data.frame(time=seq(0,300,by=2),y=NA),
              times="time",
              t0=-6*7,
              rprocess=euler.sim(
                step.fun="_blowfly_simulator",
                delta.t=1
                ),
              paramnames=c("log.P","log.N0","log.delta","log.sigma.P","log.sigma.d","tau","log.sigma.y"),
              statenames=c("N1","R","S","e","eps"),
              obsnames=c("y"),
              measurement.model=y~norm(mean=N1,sd=exp(log.sigma.y)),
              initializer=function (params, t0, ...) {
                ntau <- as.integer(params["tau"])+1
                ninit <- round(exp(params["log.N0"]))
                n <- rep(ninit,times=ntau)
                names(n) <- paste("N",seq_len(ntau),sep="")
                c(n,R=0,S=0,e=0,eps=0)
              }
              ),
         params=c(
           log.P=log(100/14),       # log recruitment rate
           log.delta=log(2.5/14),   # log mortality rate
           log.N0=log(400), # log density-dependence parameter (and initial condition)
           log.sigma.P=log(0.5),        # log recruitment noise level
           log.sigma.d=log(0.5),        # log survival noise level
           log.sigma.y=log(5),          # log measurement error SD
           tau=14                       # development delay (integer)
           ),
         nsim=1,
         seed=73691676L
         ) -> blowfly.sim

