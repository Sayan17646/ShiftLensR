#' Validate a tabular dataset
#'
#' Checks that input is a data frame with at least one row and column,
#' non-empty unique column names, and supported atomic columns.
#'
#' @param data An object expected to be a data frame.
#' @return Invisibly returns `TRUE`; invalid input raises an informative error.
#' @export
sl_validate <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("`data` must have at least one row and one column.", call. = FALSE)
  }
  column_names <- names(data)
  if (is.null(column_names) || anyNA(column_names) || any(!nzchar(trimws(column_names)))) {
    stop("All columns must have non-empty names.", call. = FALSE)
  }
  if (anyDuplicated(column_names)) {
    stop("Column names must be unique.", call. = FALSE)
  }
  supported <- vapply(data, function(column) {
    is.atomic(column) && is.null(dim(column))
  }, logical(1))
  if (!all(supported)) {
    stop("Columns must be one-dimensional atomic vectors.", call. = FALSE)
  }
  invisible(TRUE)
}
