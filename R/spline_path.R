#' spline_path
#'
#' A function that simplify a given path; remove points by splining the curve.
#'
#' @param x A vector that contains the x coordinates from the points that defines the path
#' @param y A vector that contains the y coordinates from the points that defines the path
#' @param n The number of points you want to keep after spline
#' @param t The order of the points that defines the path; default is \code{seq_along(x)}
#' @param method Method from spline function. It precise the type of spline to use.
#'               Possible values are 'fmm', 'natural', 'periodic', 'monoH.FC' and 'hyman'.
#'
#' @return A data frame of the new points that defines the path
#'
#' @export

spline_path <- function(x, y, n, t = seq_along(x), method = "natural") {
  new_t <- seq(min(t), max(t), length.out = n)           #add a time composant to spline path
  new_x <- spline(t, x, xout = new_t, method = method)$y
  new_y <- spline(t, y, xout = new_t, method = method)$y
  data.frame(x = new_x, y = new_y)
}
