#' Profile dataset columns
#'
#' Returns one row per feature with type, missingness, cardinality, and
#' numeric summaries where applicable.
#'
#' @param data A data frame to profile.
#' @return A data frame of feature summaries with class `shiftlens_profile`.
#' @export
sl_profile <- function(data) {
  sl_validate(data)
  rows <- lapply(names(data), function(feature) {
    column <- data[[feature]]
    present <- column[!is.na(column)]
    numeric_column <- is.numeric(column) && !is.factor(column)
    data.frame(
      feature = feature,
      type = if (is.factor(column)) "factor" else class(column)[1L],
      missing = sum(is.na(column)),
      missing_rate = mean(is.na(column)),
      unique = length(unique(present)),
      mean = if (numeric_column && length(present)) mean(present) else NA_real_,
      sd = if (numeric_column && length(present) > 1L) stats::sd(present) else NA_real_,
      min = if (numeric_column && length(present)) min(present) else NA_real_,
      median = if (numeric_column && length(present)) stats::median(present) else NA_real_,
      max = if (numeric_column && length(present)) max(present) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  attr(result, "n_rows") <- nrow(data)
  class(result) <- c("shiftlens", "shiftlens_profile", "data.frame")
  result
}

#' Summarize a ShiftLensR profile
#'
#' @param object A result from [sl_profile()].
#' @param ... Reserved for method compatibility.
#' @return A one-row data frame summarizing dataset dimensions and missingness.
#' @method summary shiftlens
#' @export
summary.shiftlens <- function(object, ...) {
  if (!inherits(object, "shiftlens_profile")) {
    stop("`object` must be a result from `sl_profile()`.", call. = FALSE)
  }
  data.frame(
    features = nrow(object),
    rows = attr(object, "n_rows"),
    total_missing = sum(object$missing),
    features_with_missing = sum(object$missing > 0L),
    numeric_features = sum(!is.na(object$mean)),
    stringsAsFactors = FALSE
  )
}
