library(pomp)

data(ricker)

set.seed(6457673L)
z <- as.numeric(data.array(ricker))

pb <- probe(
            ricker,
            probes=list(
              probe.marginal(
                             var="y",
                             transform=sqrt,
                             ref=z,
                             diff=1,
                             order=3
                             ),
              probe.acf(
                        var="y",
                        lags=c(0,1,3,5)
                        ),
              mean=probe.mean(var="y",transform=sqrt)
              ),
            nsim=1000,
            seed=838775L
            )
pb@datvals
summary(pb)
plot(pb)

pbm <- probe.match(pb,method="sannbox",maxit=1000,trace=3,est=c("log.r","log.sigma"))
