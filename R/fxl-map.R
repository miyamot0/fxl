#' var_map
#'
#' This helper function maps out relationships to be parsed later on
#'
#' @param ... map expressed relationships out
#'
#' @importFrom rlang enexprs
#'
#' @returns list of exprs to map variables to plotting methods
#'
#' @export
var_map <- function(...) {
  enexprs(...)
}
