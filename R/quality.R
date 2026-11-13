#' Diagnose common data-quality issues
#'
#' Reports feature missingness, constant columns, and fully duplicated rows.
#'
#' @param data A data frame to inspect.
#' @return A data frame with `issue`, `feature`, `count`, and `rate` columns.
#' @export
sl_quality <- function(data) {
  sl_validate(data)
  records <- list()
  for (feature in names(data)) {
    column <- data[[feature]]
    missing_count <- sum(is.na(column))
    if (missing_count > 0L) {
      records[[length(records) + 1L]] <- data.frame(
        issue = "missing_values", feature = feature, count = missing_count,
        rate = missing_count / nrow(data), stringsAsFactors = FALSE
      )
    }
    observed <- unique(column[!is.na(column)])
    if (length(observed) == 1L) {
      records[[length(records) + 1L]] <- data.frame(
        issue = "constant_feature", feature = feature, count = nrow(data),
        rate = 1, stringsAsFactors = FALSE
      )
    }
  }
  duplicate_count <- sum(duplicated(data))
  if (duplicate_count > 0L) {
    records[[length(records) + 1L]] <- data.frame(
      issue = "duplicate_rows", feature = NA_character_, count = duplicate_count,
      rate = duplicate_count / nrow(data), stringsAsFactors = FALSE
    )
  }
  if (!length(records)) {
    return(data.frame(issue = character(), feature = character(), count = integer(), rate = numeric()))
  }
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  result
}
