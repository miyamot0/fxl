#' var_map
#'
#' This helper function maps out relationships to be parsed later on
#'
#' @param ... map expressed relationships out
#'
#' @return list of exprs
#' @importFrom rlang enexprs
#' @export
var_map <- function(...) {
  enexprs(...)
}
