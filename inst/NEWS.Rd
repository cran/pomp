\name{NEWS}
\title{News for package `pomp'}
\section{Changes in \pkg{pomp} version 3.2}{
  \itemize{
    \item The help pages have been improved, with an description of the overall package structure and pages on basic model components, elementary algorithms, and estimation algorithms.
    \item A note to Windows users on avoiding certain C snippet compilation errors has been added to the help pages.
    \item The behavior of \code{profile_design} has changed.
    Previously, the same random (or sub-random) sequence was used for all non-profile parameters.
    Now, for each profile slice, a distinct set of points is generated.
    \item \code{profileDesign}, \code{sliceDesign}, \code{runifDesign}, and \code{sobolDesign} have been deprecated and will be removed in a future release.
    They are replaced by \code{profile_design}, \code{slice_design}, \code{runif_design}, and \code{sobol_design}, respectively.
  }
}
\section{Changes in \pkg{pomp} version 3.1}{
  \itemize{
    \item A new \code{saved.states} method allows one to extract the saved states from a particle filter computation.
    \item All instances of \code{cond.loglik} have been changed to \code{cond.logLik}, to remove a common source of typographical error.
    \item A new sequential importance sampling algorithm has been implemented as \code{wpfilter}.
    This is a generalization of the algorithm in \code{pfilter} in that it tracks the weights of the particles and allows the user to customize the resampling scheme.
    \item In \code{mif2}, the specification of particle numbers, \code{Np}, has changed.
    When \code{Np} is supplied as a function, \code{Np(0)} is the requested number of particles at the beginning of the time series.
    The previous behavior was that \code{Np(1)} specified the requested number of particles.
    This behavior now matches that of the other particle filtering algorithms, \code{pfilter}, \code{bsmc2}, and \code{pmcmc}.
    \item \code{bsmc2} can now accept a variable number of particles, as do the other particle-filter based algorithms \code{pfilter}, \code{pmcmc}, and \code{mif2}.
    \item The internal \code{systematic_resample} function now allows the user to specify the number of samples desired.
    Previously, these were always equal to the number of weights supplied.
    \item As promised from version 2.4.1, the \code{tol} and \code{max.fail} arguments have been removed completely from all particle-filtering algorithms, including \code{pfilter}, \code{pmcmc}, \code{bsmc2}, and \code{mif2}.
    See the \href{https://kingaa.github.io/pomp/blog.html}{\pkg{pomp} news blog} (\url{https://kingaa.github.io/pomp/blog.html}) for more information.
    \item The long-deprecated functions \code{onestep.dens}, \code{onestep.sim}, \code{discrete.time.sim}, \code{euler.sim}, \code{gillespie.sim}, \code{gillespie.hl.sim}, \code{conv.rec}, and \code{values}  have been removed.
    These have been replaced as follows
    \tabular{ll}{
      \code{onestep.dens} \tab direct specification of \code{dprocess} component \cr
      \code{onestep.sim} \tab \code{onestep} \cr
      \code{discrete.time.sim} \tab \code{discrete_time} \cr
      \code{euler.sim} \tab \code{euler} \cr
      \code{gillespie.sim} \tab \code{gillespie} \cr
      \code{gillespie.hl.sim} \tab \code{gillespie_hl} \cr
      \code{conv.rec} \tab \code{traces} \cr
      \code{values} \tab \code{as.data.frame} or \code{as(x,"data.frame")} \cr
    }
    \item Trap error from non-finite likelihoods in \code{pmcmc}.
    \item New test of \code{pmcmc} codes in \file{tests/issue109.R}.
    \item Trap error from missing accumulator variables in \code{trajectory}.
  }
}
\section{Changes in \pkg{pomp} version 2.8}{
  \itemize{
    \item As promised, the default value of \code{tol} in the particle-filtering algorithms \code{pfilter}, \code{pmcmc}, \code{mif2}, and \code{bsmc2} has been changed to zero.
    A warning continues to be issued if \code{tol} is set to anything other than 0.
    In a future release, the option to choose a nonzero tolerance will be removed entirely.
    \item The Euler step size in \code{sir} is now adjustable.
    \item In traces and diagnostic plotting methods, the \code{nfail} variable (tracking numbers of filtering failures) has been dropped.
  }
}
\section{Changes in \pkg{pomp} version 2.7}{
  \itemize{
    \item Another documentation bug was fixed.
  }
}
\section{Changes in \pkg{pomp} version 2.6}{
  \itemize{
    \item A documentation bug, identified by K. Hornik, was fixed.
  }
}
\section{Changes in \pkg{pomp} version 2.5}{
  \itemize{
    \item In anticipation of forthcoming algorithmic changes, the \code{tol} and \code{max.fail} arguments of \code{pfilter}, \code{bsmc2}, \code{pmcmc}, and \code{mif2} are now deprecated.
    A warning is issued if \code{tol} is set to anything other than 0.
    In a future release, these options will be removed entirely: the behavior will be similar to that obtained in the present release by setting \code{tol=0}.
    \item A new example illustrating the \code{accumvars} argument has been provided.
    \item Internal computations now use increased precision for means, variances, and certain calculations with likelihoods.
    \item A new help page, \code{pomp_examples}, lists the various example datasets and pomp objects provided with the package.
  }
}
\section{Changes in \pkg{pomp} version 2.4}{
  \itemize{
    \item The source codes underlying specification of basic model components via \R functions has been reworked to remove reliance on deep \code{PROTECT} stacks.
    Thanks to Thomas Kalibera for showing the way!
    \item A bug in the documentation for \code{filter.traj}, pointed out by Pierre Jacob, has been fixed.
  }
}
\section{Changes in \pkg{pomp} version 2.3}{
  \itemize{
    \item The C API for \pkg{pomp} is no longer explained only in comments in the \dQuote{pomp.h} header file.
    It is now described and explained in the new \href{https://kingaa.github.io/pomp/vignettes/C_API.html}{C API vignette}.
    \item The new \code{pomp_cdir} global option allows one to set the directory holding C snippet files globally.
    \item A bug caused by re-use of the \code{cfile} argument has been fixed.
    Re-use now triggers an error, with an informative message.
    \item Some documentation improvements.
  }
}
\section{Changes in \pkg{pomp} version 2.2}{
  \itemize{
    \item A bug in \code{runifDesign} (and therefore \code{profileDesign} when \code{type="runif"}) has been fixed.
    \item There are now \code{plot} methods for objects of classes \sQuote{probe_match_objfun} and \sQuote{spect_match_objfun}.
  }
}
\section{Changes in \pkg{pomp} version 2.1}{
  \itemize{
    \item The \code{melt} function can now be applied to arbitrary \sQuote{pomp} objects.
    \item A new function, \code{runifDesign} allows construction of design matrices using samples from multivariate uniform distributions.
    \item The \code{profileDesign} function takes a new argument, \code{type}, with two options:
    \code{"sobol"}, the default, constructs the design using \code{sobolDesign}, as before.
    \code{"runif"} uses the new \code{runifDesign} function.
    \item A new example, comprising models and data for the 2014--2016 west African Ebola virus disease outbreak, is included.
    See \code{?ebola} for details.
    \item It is now possible to adjust the observation times and zero-time in the \code{sir} and \code{sir2} examples.
    One can also change the default RNG seed used in generating the simulated data.
    \item The codes underlying the \code{rprocess} workhorse have been streamlined to obviate an unnecessary copy operation.
    As a result, the interface to the \code{rprocess} workhorse has been changed.
    \item The interface to the new \code{flow} workhorse has been changed to match.
    \item The new \code{flow} workhorse allows integration/iteration of the deterministic skeleton at arbitrary times from an arbitrary initial condition.
    \item When \code{coef} is applied to a \sQuote{listie}, the result can be more usefully \code{melt}ed.
    \item The probe-names returned by \code{probe.acf} and \code{probe.ccf} have changed.
    \item A bug, whereby the \sQuote{coef} method from other packages was masked, has been fixed.
    \item Errors in the documentation have been corrected.
    \item There is a new \code{as.data.frame} method for \sQuote{listie}s.
    \item Some problems with the exposure of internal objects for package developers have been corrected.
    \item The \pkg{magrittr} pipe operator, \code{\%>\%}, is now re-exported.
    \item More examples have been included.
    \item Minor changes to error messages.
    \item The names of the \sQuote{objfun} methods have been changed to \code{probe_objfun}, \code{spect_objfun}, \code{traj_objfun}, and \code{nlf_objfun}.
    \item The package directory structure has been modified to include a new \file{examples} directory, which holds the examples used in the documentation.
    \item The names of the \sQuote{objfun} methods have been shortened.
    \item It is now permissible that the sequence of observation times (and sequences of covariate times) be merely non-decreasing sequences.
    Previously, \pkg{pomp} insisted on strictly increasing time sequences.
    \item The default cooling schedule (\code{cooling.type}) in \code{mif2} is now \dQuote{geometric}, in contrast to \dQuote{hyperbolic}, as before.
    \item \pkg{pomp} now requires at least \R version 3.5.
    \item The \code{zeronames} argument has been renamed \code{accumvars}.
    \item In the course of ordinary work, one should never need to interact with the low-level \code{pomp} constructor function now.
    \item The \code{pompExample} function has been dropped.
    The various examples are now included as regular functions that construct \sQuote{pomp} objects.
    \item The \code{bbs} example has been dropped.
    \item One can now modify basic model components at (almost) any stage in a pipeline of \pkg{pomp} computations.
    All the top-level inference functions, plus \code{probe}, \code{pfilter}, and \code{simulate} now allow modification of basic model components.
    \item The \code{rprocess} plugins \code{onestep.sim}, \code{discrete.time.sim}, \code{euler.sim}, \code{gillespie.sim}, and \code{gillespie.hl.sim} have been renamed \code{onestep}, \code{discrete_time}, \code{euler}, \code{gillespie}, and \code{gillespie_hl}, respectively.
    The old usage is available but deprecated.
    \item One can now call \code{simulate} on a model without reference to any data.
    \item The covariate table facility (\code{covariate_table}) now supports piecewise constant interpolation.
    \item The syntax with which one includes covariates via \code{covariate_table} has become more flexible.
    Specifically, the arguments to \code{covariate_table} are now evaluated sequentially, so that later ones can depend on earlier ones.
    Once evaluated, the covariates are bound column-wise into a single data frame.
    \item The manner in which one writes \R functions to specify basic model components has been totally changed.
    Before, one wrote functions that took specific arguments such as 'x', 'params', and 'covars'.
    Now, one writes such functions with any or all state variables, observables, covariates, and/or time as arguments.
    \item \code{simulate} now returns more informative results when simulations from multiple parameter sets are simultaneously computed.
    Specifically, if \code{params} has column names, these are used to identify the resulting simulations.
    Thus when \code{format = "pomps"} (the default), the names of the resulting list will be constructed from the column names of \code{params}.
    Likewise, when \code{format = "arrays"}, the resulting arrays will have informative column names;
    When \code{format = "data.frame"}, the identifier variable will make use of the column names.
    \item The basic particle filter, \code{pfilter}, has a simpler mode of operation:
    \code{params} should be a single parameter set only.
    That is, it is no longer possible to pass a matrix of parameters to \code{pfilter}.
    \item The Liu-West algorithm, \code{bsmc2}, has a simpler mode of operation:
    \code{params} should be a single parameter set only:
    the SMC particles are drawn from \code{rprior}.
    \item The \code{as.data.frame}, \code{states}, and \code{obs} options for \code{simulate} have been done away with.
    One now chooses the format of the returned simulations via the \code{format} argument.
    When one simulates at multiple parameter sets, list names, array \code{dimnames}, or identifying variables in the output data frame help to correlate simulations with parameter sets.
    \item The \code{as.data.frame} argument to \code{trajectory} has been removed in favor of a new \code{format} argument that allows one to choose between receiving the results in the form of an \code{"array"} or a \code{"data.frame"}.
    \item All the basic model components now have defaults.
    \itemize{
      \item The default \code{rinit} behavior remains as it was:
      it assumes the initial state distribution is concentrated at a point mass determined by parameters with \dQuote{\code{.0}} or \dQuote{\code{_0}} suffices.

      \item The default process model is \dQuote{missing}: calls to \code{dprocess} and \code{rprocess} will result in missing values (\code{NA}).
      \item The default measurement model is \dQuote{missing} as well.
      \item The default prior is flat and improper:
      all calls to the default \code{dprior} result in \code{1} (\code{0} if \code{log = TRUE}, and all calls to \code{rprior} result in \code{NA}.
      \item The default skeleton is missing.
      \item The default parameter transformations remain the identity.
    }
    \item The \code{transform} argument present in many \pkg{pomp} algorithms has been removed.
    Parameter transformations are now automatically performed when appropriate.
    The default parameter transformation remains the identity.
    \item The old \code{probe.match} and \code{traj.match} functions have been removed.
    The new approach to parameter estimation based on numerical optimization involves constructing stateful objective functions.
    There are now three of these: \code{traj.match.objfun}, \code{probe.match.objfun}, and \code{spect.match.objfun}.
    \item The documentation system has been thoroughly overhauled.
    \item In \code{skeleton}, the \code{t} argument has been replaced by \code{times}, to make this uniform with the other workhorse functions.
    \item Covariates are now included using the \code{covariate_table} construction, passed via the \code{covar} argument to \code{pomp}.
    The \code{tcovar} argument is no longer used.
    \item Instead of providing separate \code{fromEstimationScale} and \code{toEstimationScale} arguments to \code{pomp}, one now provides a single \code{partrans} argument constructed by means of the new function \code{parameter_trans}.
    In addition, references to \dQuote{fromEstimationScale} and \dQuote{toEstimationScale} have been abbreviated to \dQuote{fromEst} and \dQuote{toEst}, respectively, throughout the package.
    \item When writing C snippets for parameter transformations, the notation \code{T_p} refers to the value of \code{p} on the estimation scale.
    Thus, if parameter \eqn{alpha} is to be log-transformed for estimation, the \code{toEst} snippet would contain the line \code{T_alpha = log(alpha);} and the \code{fromEst} snippet would include \code{alpha = exp(T_alpha);}.
    \item The \code{parameter_trans} function now takes optional arguments \code{log}, \code{logit}, and \code{barycentric}, with names of parameters to be log-, logit-, and log-barycentric transformed, respectively.
    \item The \code{measurement.model} argument to \code{pomp} has been removed.
    It is now necessary to specify the measurement model directly using \code{rmeasure} and/or \code{dmeasure}.
    \item The \code{initializer} is no longer referred to by that name.
    It is now \code{rinit}, since it draws from the distribution of the latent state at the initial time.
    The low-level function \code{init.state} has been replaced by \code{rinit}.
    \item The \code{conv.rec} method has been deprecated, replaced by the new \code{traces} method.
    \item The \code{euler.sir} example has been renamed \code{sir}.
    \item The \code{gillespie.sir} example has been renamed \code{sir2}.
    \item The \code{covmat} method can now be applied to a \code{probed.pomp} object.
    It returns the estimated covariance matrix of the probes.
    \item When \code{trajectory} calls on \pkg{deSolve} routines to numerically integrate a model vectorfield, more informative error messages are generated, and diagnostics are printed when \code{verbose = TRUE}.
    \item The \code{show} method applied to pomp objects is now quite terse.
    To obtain detailed information, the \code{spy} method is available.
    \item The \code{spy} method now returns the information formerly displayed in a call to \code{show}, in addition to the C files associated with any C snippets.
    \item The use of \code{$} methods to access the slots of \pkg{pomp} S4 objects has now been removed.
    These classes are no longer exported (for the most part).
    \item The \code{transform} argument to the \code{probe.quantile} function has been removed (as it is entirely redundant!).
    \item The old and deprecated \code{mif} and \code{bsmc} methods have been removed.
    }
}
\section{Changes in \pkg{pomp} version 1.19}{
  \itemize{
    \item The \code{obs} and \code{states} arguments of \code{simulate} are deprecated and will be removed in a forthcoming release.
    \item The C-level interface for \code{apply_probe_sims} has changed: the new argument \code{rho} must be the \dQuote{package:pomp} environment.
    \item A bug in \code{pfilter} (and hence \code{pmcmc}) when \code{filter.traj=TRUE} was discovered and fixed.
    In the buggy version, the filter trajectory was sampled with incorrect weights.
    \item Timo Voipio contributed a pull request that traps the error resulting from \code{sannbox} being passed a \code{candidate.dist} that is not a function.
    \item \code{show} and \code{print} methods have been made very terse:
    they simply report on the class of the object in question.
    To see more detailed information about a \sQuote{pomp} object, use \code{spy}.
    \item New (unexported) \code{solibs<-} method allows developers of packages extending \pkg{pomp} to incorporate C snippets into object that contain \sQuote{pomp} objects.
    \item New \code{verbose} option in \code{trajectory} gives diagnostic information from \pkg{deSolve} integration routines.
    Of course, this is pertinent only if the model skeleton is a vectorfield.
    \item Bug in the display of \code{istate} when \code{deSolve::ode} errors has been fixed.
    \item New \code{hitch} function facilitates construction of \code{pomp.fun} objects from \R functions, C snippets, and links to external libraries.
    \item Native routines needed to access \code{pomp.fun} objects in C are now registered for use by other packages.
    \item \code{coef(object) <- NULL}, where \code{object} is of class \sQuote{pomp}, erases any parameters stored in \code{object}.
    \item The \code{euler.sir}, \code{gillespie.sir}, \code{blowflies}, \code{ricker}, \code{bbs}, \code{dacca}, and \code{rw2} examples are all now implemented using C snippets.
    \item A new \file{pomp_defines.h} header is provided for developers of packages that depend on \pkg{pomp}.
    \item Internally, \code{rprocess} is no longer stored as an \R function.
    The use of plugins is now required for the implementation of the \code{rprocess} component.
    Currently, there are five plugins available:
    \code{onestep.sim}, \code{discrete.time.sim}, \code{euler.sim}, \code{gillespie.sim}, and \code{gillespie.hl.sim}.
    \item The new \code{hitch} function automates the building and linking of C snippets and \R functions into \code{pomp.fun}s.
    \item The handling of \code{dprocess} has changed.
    The \code{onestep.dens} plugin is now deprecated and will soon be removed.
    \code{dprocess} is now specified directly using either a C snippet or an \R function, in much the same way that, for example, \code{dmeasure} is specified.
    \item The old \sQuote{forward} and \sQuote{inverse} options for the \code{dir} argument of \code{partrans} have been dispensed with.
    The much more descriptive options \sQuote{toEstimationScale} and \sQuote{fromEstimationScale} remain.
    \item The C snippet compiling facilities have been thoroughly reworked.
    \item \code{onestep.dens} can now take a \code{Csnippet} specification of the process-model transition density.
  }
}
\section{Changes in \pkg{pomp} version 1.18}{
  \itemize{
    \item All inference functions can now take a list in the \code{start} or \code{params} arguments.
    \item When altering parameters in a call to \code{probe} on a \code{probed.pomp} object, the new parameters were ignored.
    This bug has been fixed.
    \item New \code{spy} method displays the C snippet file(s) associated with a \code{pomp} object.
    \item The long-deprecated \code{seed} argument to \code{bsmc} and \code{bsmc2} has been removed.
    \item More work to obviate warnings generated by T. Kalibera's latest \code{rchk} checks.
  }
}
\section{Changes in \pkg{pomp} version 1.17}{
  \itemize{
    \item Messages from the compiler resulting from attempts to compile C snippets are now suppressed on all platforms except in case of failure.
    Setting \code{verbose=TRUE} forces display of these messages.
    \item Source codes have been revisited to obviate warnings generated by T. Kalibera's latest \code{rchk} checks.
    \item Fix a minor bugs in \code{spect}.
    \item Improve test coverage.
    \item \code{spect.match} has been refactored, with small changes to the interface.
    \item \code{pomp} now handles specification of workhorse functions (i.e., \code{rprocess}, \code{dprocess}, \code{rmeasure}, \code{dmeasure}, \code{initializer}, \code{skeleton}, \code{rprior}, \code{dprior}, \code{fromEstimationScale}, \code{toEstimationScale}) as \code{NULL} more consistently.
    \item Fix bug in systematic resampler when \code{np != nw}.
    \item Fix bug in \code{conv.rec} when only one parameter is requested.
    \item Correct and improve documentation for \code{conv.rec}.
    \item \code{coef<-} can now take a list of parameters:
    it simply applies \code{unlist} to turn the list into a numeric vector.
    \item Simplify \code{conv.rec}.
    \item In \code{pfilter}, \code{Np} is now a required argument unless \code{params} is specified as a matrix of particles.
    Thanks to Carles Breto for catching the unintended behavior.
  }
}
\section{Changes in \pkg{pomp} version 1.16}{
  \itemize{
    \item The bug reported in issue #57 has been fixed.
    \item A bug arising when building pomp objects with Csnippets in certain Windows configurations has been fixed.
    \item Issue #56, in which covariate names are discarded when coercing a pomp object to a data frame, has been fixed.
    Thanks to Eamon O'Dea for reporting this bug.
    \item More informative error messages are given when \code{bake} or \code{freeze} return \code{NULL}.
    In such a case, these functions now generate a warning and return a character-string message, with attributes.
    \item Improve documentation of \code{traj.match}, in response to Issue #57.
    \item A new facility, \code{gillespie.hl.sim}, allows implementation of Gillespie's algorithm for exact stochastic simulation of continuous-time Markov processes via a streamlined C snippet interface.
    \item The old facility for Gillespie simulations, \code{gillespie.sim}, now accepts C snippets for faster calculation of event rates.
    \item The old \code{skeleton.type} and \code{skelmap.delta.t} arguments of the \code{pomp} constructor, which have generated error messages since version 1.9.2, have been done away with completely.
    \item Eamon O'Dea identified a bug (Issue #54) occurring when \code{pomp} is called with a single-row data frame.
    This has been fixed using his contributed patch.
  }
}
\section{Changes in \pkg{pomp} version 1.15}{
  \itemize{
    \item A new \code{dmultinom} function is provided in \sQuote{pomp.h}.
    This provides the probability distribution function for the multinomial distribution.
    \item Improved tests.
    \item Fixed bugs emerging in R-devel due to refactoring of underlying \R source code.
  }
}
\section{Changes in \pkg{pomp} version 1.14}{
  \itemize{
    \item Eamon O'Dea pointed out that the bug in \code{gillespie.sim}, addressed by version 1.12.3, was not completely fixed.
    This problem has now been resolved.
    In addition, covariates are now guaranteed to be evaluated at least once per observation interval.
    \item The new argument \code{hmax} to \code{gillespie.sim} allows the user to specify the maximum step size that will be taken before covariates are evaluated.
    \item The K-leap method has been removed.
    Its accuracy is low on test problems and it is difficult to make it compatible with the pomp paradigm.
    In particular, it does not readily accomodate time-varying covariates in a sensible way, nor does it lend itself to simulations at pre-specified times.
    \item \code{bspline.basis} and \code{periodic.bspline.basis} now take an optional non-negative integer argument \code{deriv}.
    When \code{deriv > 0}, the order-\code{deriv} derivative of each basis function is returned.
    \item A new function, \code{periodic_bspline_basis_eval_deriv}, has been added to the C API.
    This function gives access, at the C level, to evaluation of the derivatives of periodic B-splines.
    \item Upgrade license to GPL version 3.
    \item Fix buffer-flow problem identified by \code{valgrind}.
  }
}
\section{Changes in \pkg{pomp} version 1.13}{
  \itemize{
    \item Fix bug found by Eamon O'Dea in \code{gillespie.sim}.
    \item Fix bug in \code{onestep.sim}.
    \item The Ricker example has a new parameter, \eqn{c}, which scales the (unobserved) population size.
    By default, \eqn{c=1}, so that the behavior is as before.
    \item Improved error trapping.
  }
}
\section{Changes in \pkg{pomp} version 1.12}{
  \itemize{
    \item Some changes have been made to the use of random numbers in some codes (the stochastic simulations in \code{gillespie.sim} and the resampling in the particle-filtering codes).
    These changes will mean that calculations performed before will not be reproducible in perfect detail, but will be statistically equivalent.
    \item Minor changes in C codes to obviate warnings generated by T. Kalibera's \code{rchk} checks.
    \item The rare and fascinating \dQuote{Bug of St. Patrick} has been fixed.
    Thanks to Carles Breto for finding the bug and helping to isolate it.
    \item More informative error messages are generated when \code{dmeasure} returns a non-finite likelihood in any of the particle filtering codes.
    \item Some streamlining of codes.
  }
}
\section{Changes in \pkg{pomp} version 1.11}{
  \itemize{
    \item Callable routines are now registered.
    \item The functions \code{bake}, \code{stew}, and \code{freeze} now preserve information about the system time used in computation as an attribute of the returned object.
    In addition, these functions store information about the RNG settings.
    \item Some documentation improvements, courtesy of Sebastian Funk.
    \item Various documentation improvements.
    \item More verbosity is provided with \code{verbose=TRUE}.
    \item Better handling of \code{pomp}'s \code{times} argument.
  }
}
\section{Changes in \pkg{pomp} version 1.10}{
  \itemize{
    \item New \code{logLik} method for \code{pmcmcList} objects.
    \item The deprecated option to supply a matrix or numeric vector as the \code{data} argument to \code{pomp} have been removed and an error is now generated.
    \code{data} must be provided as either a data frame or an object of class \code{pomp}.
    See \code{?pomp} for details.
    \item The deprecated \code{skeleton.type} and \code{skelmap.delta.t} arguments to \code{pomp} have been removed.
    Using these arguments will now generate an error message.
    The correct method of specifiying the deterministic skeleton is now explained in the help pages (\code{?pomp}).
    \item Source code fixes to eliminate warnings during compilation.
    \item Improved test suite.
    \item Minor documentation correction.
  }
}
\section{Changes in \pkg{pomp} version 1.9}{
  \itemize{
    \item During particle filtering, when non-finite \code{dmeasure} values are generated, an error is generated and the offending parameters, states, data, and time are reported.
    In methods that use parameter transformations (\code{transform=TRUE}), the reported parameters were on the transformed (estimation) scale, which can be confusing.
    The error message now gives the parameters on the natural (model) scale.
    \item Modified \file{src/ssa.c} to fix UBSAN errors.
    \item C codes have been modified to take advantage of changes to the \R API (R SVN REVISION 71180).
    \item A bug in \code{mif2} arising when filtering fails in the final timestep has been fixed.
    In this case, when all particles are deemed inconsistent with the data, we use an unweighted mean (with a warning) in place of the default weighted mean.
    \item C codes have been modified to take advantage of changes to the \pkg{R} API (R SVN REVISION 71180).
    \item Modified \file{src/ssa.c} to fix UBSAN errors.
  }
}
\section{Changes in \pkg{pomp} version 1.8}{
  \itemize{
    \item There is a new option, \code{shlib.args}, to \code{pomp}.
    This allows arbitrary options to be passed to the compiler when C snippets are used.
    In particular, one can link a library of C snippets against a precompiled static library.
    \item In Sobol' sequence generation (\code{sobolDesign}, \code{profileDesign}), we now use the suggestion of Joe & Kuo (2003) to choose a better sequence start point.
    \item The generation of Sobol' sequences (\code{sobolDesign}, \code{profileDesign}) is now performed using the \pkg{NLopt} routines written in C instead of the original \acronym{TOMS} 659 FORTRAN codes.
    The copyright notices are located in the source code.
    \item The stochastic simulation algorithms underlying the Gillespie and K-leap methods (\code{gillespie.sim}, \code{kleap.sim}) have been refactored in C.
    In addition, errors due to accumulation of round-off error have been repaired.
    The result will be more accurate, but somewhat slower when there are a very large number of reactions.
    \item Fixed a bug in \code{plot} on \code{pomp} objects with more than 10 variables to plot.
    \item Refactored \code{rw.sd} for greater stability.
    \item Modified the \dQuote{dacca} example to use C snippets.
    \item Made Sobol' codes more robust.
    \item Modified the demos to use best practices.
    \item Further improvements to the test suite.
    Code coverage is now 100\%.
  }
}  
\section{Changes in \pkg{pomp} version 1.7}{
  \itemize{
    \item Improvement of the test suite and of error messages.
    \item Improve the \code{show} and \code{print} methods for \code{pomp} objects.
    \item Some internal refactoring.
    \item Improve error messages for missing basic components.
    \item Fix typos in one of the help-page examples.
    \item Several obscure bug-fixes.
    \item Add some references.
  }
}
\section{Changes in \pkg{pomp} version 1.6}{
  \itemize{
    \item Major revisions to the package help pages and manual.
    The separate documentation on C snippets and process-model plugins has been merged into the main \code{pomp} help page, which now has comprehensive instructions on building a \code{pomp} object.
    \item Update to the package help page.
    \item New methods for the Ensemble Kalman Filter (\code{enkf}) and Ensemble Adjustment Kalman Filter (\code{eakf}) have been added to the package.
    \item There is a new plugin for \code{rprocess} called \code{kleap.sim}.
    This implements the so-called \dQuote{K-leap method} of Cai & Xu (2007).
    \item Major overhaul of error handling system and to error and warning messages.
    Many more errors are now trapped and reported with more informative messages.
    \item The matrix and numeric vector options for the \code{data} argument to \code{pomp} are now deprecated and will be removed in a future release.
    In calls to \code{pomp}, \code{data} should be either a data frame or a \code{pomp}-class object.
    \item In \code{mif2}, it is now required that \code{Nmif>0}.
    \item Redundant \code{bootstrap} argument to \code{nlf} has been removed.
    \item Many improvements to the test suite to obtain better code coverage.
    \item Fixed bugs that might arise in rare circumstances in \code{simulate} and \code{eulermultinom} functions.
    \item Fixed a bug in \code{pompExample}.
    \item Fixed several small, rare bugs found as a result of improved testing.
    \item Fixed bug in specification of skeleton in pompExamples.
    \item \code{pomp} is no longer an S4 method.
    Its new status as an ordinary function abbreviates and consolidates the code and simplifies the documentation.
    \item A bug restricting access in C code to \code{userdata} has been fixed.
    \item Fixed bug in \code{pompExample(...,show=TRUE)}.
  }
}
\section{Changes in \pkg{pomp} version 1.5}{
  \itemize{
    \item A better interface for specifying a model's deterministic skeleton is provided.
    One specifies \code{skeleton=map(f,delta.t)} for a discrete-time skeleton (a map) and \code{skeleton=vectorfield(f)} for a continuous-time skeleton (a vectorfield).
    The old arguments \code{skeleton.type} and \code{skelmap.delta.t} are deprecated and will be removed in a future release.
    \item The \code{pfilter} help page has been improved.
    Specifically, the discussion of filtering failures is better.
    \item Miscellaneous improvements to the help pages.
    \item \code{mif2} no longer computes filter means.
    \item The new argument \code{show} of \code{pompExamples} allows one to display the example code instead of executing it.
    \item The minimum version of \R supported is now 3.1.2.
    \item \code{init.state} now has the optional argument \code{nsim}.
    Using this, one can request multiple initial state vectors per parameter vector.
    \item \code{pfilter} now uses less memory when it is asked to run at a single point in parameter space.
    \item A bug associated with the \code{rw.sd} argument to \code{mif2} on Windows platforms has been fixed.
    \item The \code{method="mif2"} option to \code{mif} has been removed.
    Use \code{mif2} instead.
    \item The \code{particles} method (rarely if ever used), has been removed to streamline the \code{mif} codes.
    \item The \code{method="mif2"} option to \code{mif} is now deprecated.
    It will be removed in a future release.
    Use \code{mif2} instead.
  }
}
\section{Changes in \pkg{pomp} version 1.4}{
  \itemize{
    \item Following the publication of the Journal of Statistical Software announcement paper, the \file{CITATION} has been modified to include the appropriate citation.
    Please use \code{citation("pomp")} to learn about how to cite \pkg{pomp} correctly.
    \item \code{conv.rec} on the results of a \code{mif2} computation now play nicely with \code{reshape2::melt}.
    \item It is now possible to direct \code{pomp} to place \code{Csnippet} codes in a specified location.
    This is accomplished via the new \code{cdir} and \code{cfile} arguments to \code{pomp}.
    \item When several concurrent \R processes attempt to simultaneously compile \code{Csnipet} code from copies of a \code{pomp} object on the same machine, a race condition exists.
    This release fixes this by ensuring that \code{Csnippet} codes are written and compiled in a process-specific directory.
  }
}
\section{Changes in \pkg{pomp} version 1.3}{
  This release removes several long-deprecated features.
  \itemize{
    \item The \code{seed} argument to \code{pfilter} has been removed.
    Use \code{freeze} to obtain similar functionality.
    \item The redundant \code{pars} argument to \code{mif} has been removed.
    Names of parameters to be estimated are taken from the nonzero entries of \code{rw.sd}.
    Initial-value parameters are designated using the \code{ivps} argument.
    \item The old \code{parameter.transform} and \code{parameter.inv.transform} arguments to \code{pomp} have been removed.
    They were superseded by the arguments \code{fromEstimationScale} and \code{toEstimationScale} in version 0.65-1.
    \item Some internal changes to \code{mif2} by C. Breto, in support of panel methods under development.
  }
}
\section{Changes in \pkg{pomp} version 1.2}{
  \itemize{
    \item Bug fix: for POMPs with a scalar state variable, the \code{filter.mean}, \code{pred.mean}, and \code{pred.var} methods dropped the singleton dimension of the respective arrays, in conflict with the assumptions of certain \code{plot} methods.
    This resolves issue #3.
    \item Documentation improvements.
    Specifically, the details of how step-sizes are chosen in the \code{euler.sim}, \code{discrete.time.sim}, and \code{onestep.sim} plugins has been spelled out explicitly.
    Also, incompleteness in the \code{sannbox} documentation has been corrected.
    \item Provide \code{filter.traj} method for \code{pmcmcList} objects.
    \item Fixed a bug in \code{mif} and \code{mif2} when \code{cooling.type="hyperbolic"} and \code{cooling.fraction.50=1}.
  }
}
