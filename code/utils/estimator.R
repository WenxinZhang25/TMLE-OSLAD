# Module for updating treatment randomization and implementing causal estimation
adaptEst <- R6::R6Class(
  "adaptEst",
  inherit = t3s_Estimator,
  public = list(
    initialize = function(params = NULL, ..., est_fun) {
      if (is.null(params)) {
        params <- list()
      }
      params$est_fun <- est_fun
      private$.est_fun <- est_fun
      super$initialize(params = params, ...)
    },
    estimate = function() {
      cat("Starting Step", self$simulation$step,"\n")
      est_fun_args <- self$params
      est_fun_args$simulation <- self$simulation
      pA_new <- do.call(self$est_fun, est_fun_args)
      cat("Finishing Step", self$simulation$step,"\n")
      return(pA_new)
    }
  ),
  active = list(
    est_fun = function() {
      return(private$.est_fun)
    }
  ),
  private = list(.est_fun = NULL)
)

est_from_fun <- function(est_fun, ...) {
  return(t3s_Estimator_Functional$new(..., est_fun = est_fun))
}
