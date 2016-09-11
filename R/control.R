# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Tuning parameters for Huber M-estimation of location and scatter
#'
#' Obtain a list with tuning paramters for \code{\link{covHuber}}.
#'
#' @param prob  numeric; probability for the quantile of the
#' \eqn{\chi^{2}}{chi-squared} distribution to be used as cutoff point in the
#' Huber weight function (defaults to 0.95).
#' @param maxIterations  an integer giving the maximum number of iterations in
#' the iteratively reweighted algorithm.
#' @param tol  a small positive numeric value to be used to determine
#' convergence of the iteratively reweighted algorithm.
#'
#' @return A list with components corresponding to the arguments.
#'
#' @author Andreas Alfons
#'
#' @references
#' Huber, P.J. (1981) \emph{Robust statistics}. John Wiley & Sons.
#'
#' @seealso \code{\link{covHuber}}
#'
#' @keywords multivariate
#'
#' @export

covControl <- function(prob = 0.95, maxIterations = 200, tol = 1e-07) {
  # check supplied values
  prob <- rep(as.numeric(prob), length.out=1)
  if(!is.finite(prob)) prob <- formals$prob
  else if(prob < 0) prob <- 0
  else if(prob > 1) prob <- 1
  maxIterations <- rep(as.integer(maxIterations), length.out=1)
  if(!is.finite(maxIterations)) maxIterations <- formals()$maxIterations
  else if(maxIterations < 0) maxIterations <- 0
  tol <- rep(as.numeric(tol), length.out=1)
  if(!is.finite(tol)) tol <- formals()$tol
  else if(tol < 0) tol <- 0
  # return list of control parameters
  list(prob=prob, maxIterations=maxIterations, tol=tol)
}
