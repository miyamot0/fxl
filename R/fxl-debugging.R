#' isValidDataFrame
#'
#' @param object dataframe (hopefully)
#' @param name name for object
#'
#' @export
isValidDataFrame <- function(object = NULL, name = NULL) {
  if (is.null(object)) {
    stop(paste(
      "Parameter:",
      name,
      "should NOT be set to a null value."
    ))
  }

  if (is.data.frame(object) && nrow(as.data.frame(object)) == 0) {
    stop(paste(
      "Parameter:",
      name,
      "contains no data."
    ))
  }
}

#' isValidAestheticMapping
#'
#' @param object dataframe (hopefully)
#' @param name name for object
#'
#' @export
isValidAestheticMapping <- function(object = NULL, name = NULL) {
  if (is.null(object)) {
    stop(paste(
      "Parameter:",
      name,
      "cannot be set to a null value."
    ))
  }

  aes <- enexpr(object)

  if (!("x" %in% names(aes))) {
    stop(paste(
      "Parameter:",
      name,
      "must contain a mapping for x [",
      names(aes),
      "]"
    ))
  }

  if (!("y" %in% names(aes))) {
    stop(paste(
      "Parameter:",
      name,
      "must contain a mapping for y [",
      names(aes),
      "]"
    ))
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
    stop(paste(
      "Parameter:",
      name,
      "should NOT be set to a null value."
    ))
  }

  # Note: even a single num is basically vector of 1
  # if (!is.vector(object)) {
  #   stop(paste(
  #     "Parameter:",
  #     name,
  #     "should be a vector."
  #   ))
  # }

  if (!is.numeric(object)) {
    stop(paste(
      "Parameter:",
      name,
      "should be of a numeric type."
    ))
  }

  if (length != -1 && length(object) != length) {
    stop(paste(
      "Parameter:",
      name,
      "should have",
      length,
      "entries but has",
      length(object),
      "."
    ))
  }
}

#' isValidCharacterVector
#'
#' @param object some type of object
#' @param length expected length
#' @param name parameter name
#'
#' @export
isValidCharacterVector <- function(object = NULL, length = -1, name = NULL) {
  if (is.null(object)) {
    stop(paste(
      "Parameter:",
      name,
      "should NOT be set to a null value."
    ))
  }

  if (!is.character(object)) {
    stop(paste(
      "Parameter:",
      name,
      "should be of a character type."
    ))
  }

  # if (length != -1 && length(object) != length) {
  #   stop(paste("Parameter:",
  #              name,
  #              "should have",
  #              length,
  #              "entries but has",
  #              length(object),
  #              "."))
  # }
}


#' isValidLogicalVector
#'
#' @param object some type of object
#' @param length expected length
#' @param name parameter name
#'
#' @export
isValidLogicalVector <- function(object = NULL, length = -1, name = NULL) {
  if (is.null(object)) {
    stop(paste(
      "Parameter:",
      name,
      "should NOT be set to a null value."
    ))
  }

  if (!is.logical(object)) {
    stop(paste(
      "Parameter:",
      name,
      "should be of a logical type."
    ))
  }

  if (length != -1 && length(object) != length) {
    stop(paste(
      "Parameter:",
      name,
      "should have",
      length,
      "entries but has",
      length(object),
      "."
    ))
  }
}

#' isValidAXSCharacter
#'
#' @param object some type of object
#' @param name parameter name
#'
#' @export
isValidAXSCharacter <- function(object = NULL, name = NULL) {
  if (!is.character(object)) {
    stop(paste(
      "Parameter:",
      name,
      "must be a single-character value."
    ))
  }

  if (is.character(object) && !(object == "i" || object == "r")) {
    stop(paste(
      "Parameter:",
      name,
      "must be set to either \"i\" or \"r\""
    ))
  }
}

#' assert_input_type
#'
#' @param object some type of object
#' @param types list of object types acceptable
#' @param tag var to reference in error message
assert_input_type <- function(object, types = character(0), tag = "") {
  if (is.null(object)) stop(paste(tag,
                                "must not be NULL"))

  if (is.na(object)) stop(paste(tag,
                                "must not be NA"))

  if (!is.character(types)) stop("types must be based in characters")

  if (is.vector(object)) {
    if (!(class(object) %in% types))
      stop(paste0("Error ('", tag, "'): was of type ",
                  class(object),
                  ", check the data type supplied"))
  } else {
    if (class(object) != types)
      stop(paste0("Error ('", tag, "'): was of type ",
                  class(object),
                  ", check the data type supplied"))
  }
}
