require(pomp)

measles <- read.csv("london_measles.csv")
bp <- read.csv("london_birthpop.csv")
bp$births <- round(bp$births)

pomp(
     data=measles[c("reports","biweek")],
     times="biweek",
     t0=0,
     rprocess=discrete.time.sim(
       step.fun="_tsir_simulator"
       ),
     rmeasure="_tsir_binom_rmeasure",
     dmeasure="_tsir_binom_dmeasure",
     skeleton.map="_tsir_skeleton",
     paramnames=c("log.beta1","log.m","log.alpha","log.rho","period","degree","nseas"),
     statenames=c("S","I","theta"),
     covarnames=c("births"),
     obsnames=c("reports"),
     covar=bp[c("biweek","births")],
     tcovar="biweek"
     ) -> tsir

tsir <- window(tsir,end=520)

coef(tsir) <- c(
                log.beta=log(rep(0.0005,26))+log(rep(c(0.5,1.5),each=13)),
                log.m=log(2),
                log.alpha=log(0.9),
                log.rho=log(0.6),
                nseas=26,
                degree=1,
                period=26,
                I.0=2400,
                S.0=4000,
                theta.0=0
                )
