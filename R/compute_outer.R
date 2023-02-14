#' @title
#' Computes distance on all possible combinations
#'
#' @description
#' Computes the distance between all combination of elements in two vector. a is
#' vector of size n, and b is a vector of size m, the result, will be a matrix
#' of size(n,m)
#'
#' @param a first vector (size n)
#' @param b second vector (size m)
#' @param op operator (e.g., '-', '+', '/', ...)
#'
#' @return
#' A n by m matrix that includes abs difference between elements of vector a and b.
#'
#' @keywords internal
#'
compute_outer <- function(a, b , op) {
  return(abs(outer(a, b, op)))
}
