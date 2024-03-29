% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ricker.R
\docType{data}
\name{ricker}
\alias{ricker}
\title{Ricker model with Poisson observations.}
\usage{
ricker(r = exp(3.8), sigma = 0.3, phi = 10, c = 1, N_0 = 7)
}
\arguments{
\item{r}{intrinsic growth rate}

\item{sigma}{environmental process noise s.d.}

\item{phi}{sampling rate}

\item{c}{density dependence parameter}

\item{N_0}{initial condition}
}
\value{
A \sQuote{pomp} object containing the Ricker model and simulated data.
}
\description{
\code{ricker} is a \sQuote{pomp} object encoding a stochastic Ricker model
with Poisson measurement error.
}
\details{
The state process is \eqn{N_{t+1} = r N_{t} \exp(-c N_{t}+e_{t})}{N[t+1] =
r N[t] exp(-c N[t]+e[t])}, where the \eqn{e_t}{e[t]} are i.i.d. normal
random deviates with zero mean and variance \eqn{\sigma^2}{sigma^2}.  The
observed variables \eqn{y_t}{y[t]} are distributed as
\eqn{\mathrm{Poisson}(\phi N_t)}{Poisson(phi N[t])}.
}
\examples{
po <- ricker()
plot(po)
coef(po)
simulate(po) |> plot()
\donttest{ # takes too long for R CMD check
  ## generate a bifurcation diagram for the Ricker map
  p <- parmat(coef(ricker()),nrep=500)
  p["r",] <- exp(seq(from=1.5,to=4,length=500))
  trajectory(
    ricker(),
    times=seq(from=1000,to=2000,by=1),
    params=p,
    format="array"
  ) -> x
  matplot(p["r",],x["N",,],pch='.',col='black',
    xlab=expression(log(r)),ylab="N",log='x')
}
}
\seealso{
More examples provided with \pkg{pomp}: 
\code{\link{blowflies}},
\code{\link{childhood_disease_data}},
\code{\link{compartmental_models}},
\code{\link{dacca}()},
\code{\link{ebola}},
\code{\link{gompertz}()},
\code{\link{ou2}()},
\code{\link{pomp_examples}},
\code{\link{rw2}()},
\code{\link{verhulst}()}
}
\concept{pomp examples}
\keyword{models}
