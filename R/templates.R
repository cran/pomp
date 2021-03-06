## default templates for pomp's own C snippets.
## This is used in 'pomp.R' and 'builder.R'.

workhorse_templates <- list(
  rinit=list(
    slotname="rinit",
    Cname="__pomp_rinit",
    proto=quote(rinit(...)),
    header="\nvoid __pomp_rinit (double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars)\n{\n",
    footer="\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      states=list(
        names=quote(statenames),
        cref="__x[__stateindex[{%v%}]]"
      )
    )
  ),
  rmeasure=list(
    slotname="rmeasure",
    Cname="__pomp_rmeasure",
    proto=quote(rmeasure(...)),
    header="\nvoid __pomp_rmeasure (double *__y, const double *__x, const double *__p, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t)\n{\n",
    footer="\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      states=list(
        names=quote(statenames),
        cref="__x[__stateindex[{%v%}]]"
      ),
      obs=list(
        names=quote(obsnames),
        cref="__y[__obsindex[{%v%}]]"
      )
    )
  ),
  dmeasure=list(
    slotname="dmeasure",
    Cname= "__pomp_dmeasure",
    proto=quote(dmeasure(log,...)),
    header="\nvoid __pomp_dmeasure (double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t)\n{\n",
    footer="\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      states=list(
        names=quote(statenames),
        cref="__x[__stateindex[{%v%}]]"
      ),
      obs=list(
        names=quote(obsnames),
        cref="__y[__obsindex[{%v%}]]"
      ),
      lik=list(
        names="lik",
        cref="__lik[0]"
      )
    )
  ),
  step.fn=list(
    slotname="step.fun",
    Cname="__pomp_stepfn",
    proto=quote(step.fun(...)),
    header="\nvoid __pomp_stepfn (double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double dt)\n{\n",
    footer="\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      states=list(
        names=quote(statenames),
        cref="__x[__stateindex[{%v%}]]"
      )
    )
  ),
  rate.fn=list(
    slotname="rate.fun",
    Cname="__pomp_ratefn",
    proto=quote(rate.fun(j,...)),
    header="\ndouble __pomp_ratefn (int j, double t, double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars)\n{\n  double rate = 0.0;  \n",
    footer="  return rate;\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      states=list(
        names=quote(statenames),
        cref="__x[__stateindex[{%v%}]]"
      )
    )
  ),
  dprocess=list(
    slotname="dprocess",
    Cname="__pomp_dproc",
    proto=quote(dprocess(...)),
    header="\nvoid __pomp_dproc (double *__loglik, const double *__x1, const double *__x2, double t_1, double t_2, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars)\n{\n",
    footer="\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      before=list(
        names=quote(paste0(statenames,"_1")),
        cref="__x1[__stateindex[{%v%}]]"
      ),
      after=list(
        names=quote(paste0(statenames,"_2")),
        cref="__x2[__stateindex[{%v%}]]"
      ),
      loglik=list(
        names="loglik",
        cref="__loglik[0]"
      )
    )
  ),
  skeleton=list(
    slotname="skeleton",
    Cname="__pomp_skelfn",
    proto=quote(skeleton(...)),
    header="\nvoid __pomp_skelfn (double *__f, const double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t)\n{\n",
    footer="\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      states=list(
        names=quote(statenames),
        cref="__x[__stateindex[{%v%}]]"
      ),
      derivs=list(
        names=quote(paste0("D",statenames)),
        cref="__f[__stateindex[{%v%}]]"
      )
    )
  ),
  fromEst=list(
    slotname="fromEst",
    Cname="__pomp_from_trans",
    proto=quote(from.trans(...)),
    header="\nvoid __pomp_from_trans (double *__p, const double *__pt, const int *__parindex)\n{\n",
    footer="\n}\n\n",
    vars=list(
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      transforms=list(
        names=quote(paste0("T_",paramnames)),
        cref="__pt[__parindex[{%v%}]]"
      )
    )
  ),
  toEst=list(
    slotname="toEst",
    Cname="__pomp_to_trans",
    proto=quote(to.trans(...)),
    header="\nvoid __pomp_to_trans (double *__pt, const double *__p, const int *__parindex)\n{\n",
    footer="\n}\n\n",
    vars=list(
      covars=list(
        names=quote(covarnames),
        cref="__covars[__covindex[{%v%}]]"
      ),
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      transforms=list(
        names=quote(paste0("T_",paramnames)),
        cref="__pt[__parindex[{%v%}]]"
      )
    )
  ),
  rprior=list(
    slotname="rprior",
    Cname="__pomp_rprior",
    proto=quote(rprior(...)),
    header="\nvoid __pomp_rprior (double *__p, const int *__parindex)\n{\n",
    footer="\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      )
    )
  ),
  dprior=list(
    slotname="dprior",
    Cname="__pomp_dprior",
    proto=quote(dprior(log,...)),
    header="\nvoid __pomp_dprior (double *__lik, const double *__p, int give_log, const int *__parindex)\n{\n",
    footer="\n}\n\n",
    vars=list(
      params=list(
        names=quote(paramnames),
        cref="__p[__parindex[{%v%}]]"
      ),
      lik=list(
        names="lik",
        cref="__lik[0]"
      )
    )
  )
)
