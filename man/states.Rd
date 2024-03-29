% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/states.R
\docType{methods}
\name{states}
\alias{states}
\alias{states,ANY-method}
\alias{states,missing-method}
\alias{states,pomp-method}
\alias{states,listie-method}
\title{Latent states}
\usage{
\S4method{states}{pomp}(object, vars, ..., format = c("array", "data.frame"))

\S4method{states}{listie}(object, vars, ..., format = c("array", "data.frame"))
}
\arguments{
\item{object}{an object of class \sQuote{pomp}, or of a class extending \sQuote{pomp}}

\item{vars}{names of variables to retrieve}

\item{...}{ignored}

\item{format}{format of the returned object}
}
\description{
Extract the latent states from a \sQuote{pomp} object.
}
\seealso{
Other extraction methods: 
\code{\link{coef}()},
\code{\link{cond_logLik}()},
\code{\link{covmat}()},
\code{\link{eff_sample_size}()},
\code{\link{filter_mean}()},
\code{\link{filter_traj}()},
\code{\link{forecast}()},
\code{\link{logLik}},
\code{\link{obs}()},
\code{\link{pred_mean}()},
\code{\link{pred_var}()},
\code{\link{saved_states}()},
\code{\link{spy}()},
\code{\link{summary}()},
\code{\link{time}()},
\code{\link{timezero}()},
\code{\link{traces}()}
}
\concept{extraction methods}
