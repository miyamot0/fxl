
#' isValidDataFrame
#'
#' @param object dataframe (hopefully)
#' @param name name for object
#'
#' @export
#'
#' @examples
isValidDataFrame <- function(object = NULL, name = NULL) {
  if (is.null(object)) {
    stop(paste("Parameter:",
               name,
               "should NOT be set to a null value."))
  }

  if (is.data.frame(object) && nrow(object) == 0) {
    stop(paste("Parameter:",
               name,
               "contains no data."))
  }
}

#' isValidAestheticMapping
#'
#' @param object dataframe (hopefully)
#' @param name name for object
#'
#' @export
#'
#' @examples
isValidAestheticMapping <- function(object = NULL, name = NULL) {
  if (is.null(object)) {
    stop(paste("Parameter:",
               name,
               "cannot be set to a null value."))
  }

  aes <- enexpr(object)

  if (!("x" %in% names(aes))) {
    stop(paste("Parameter:",
               name,
               "must contain a mapping for x."))
  }

  if (!("y" %in% names(aes))) {
    stop(paste("Parameter:",
               name,
               "must contain a mapping for y."))
  }
}

#' isValidNumericVector
#'
#' @param object some type of object
#' @param length expected length
#' @param name parameter name
#'
#' @export
isValidNumericVector <- function(object = NULL, length = -1, name = NULL) {
  if (is.null(object)) {
    stop(paste("Parameter:",
               name,
               "should NOT be set to a null value."))
  }

  if (!is.vector(object)) {
    stop(paste("Parameter:",
               name,
               "should be a vector."))
  }

  if (!is.numeric(object)) {
    stop(paste("Parameter:",
               name,
               "should be of a numeric type."))
  }

  if (length != -1 && length(object) != length) {
    stop(paste("Parameter:",
               name,
               "should have",
               length,
               "entries but has",
               length(object),
               "."))
  }
}

isValidAXSCharacter <- function(object = NULL, name = NULL) {
  if (!is.character(object)) {
    stop(paste("Parameter:",
               name,
               "must be a single-character value."))
  }

  if (is.character(object) && !(object == "i" || object == "r")) {
    stop(paste("Parameter:",
               name,
               "must be set to either \"i\" or \"r\""))
  }
}
