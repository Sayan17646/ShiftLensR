#' Compare reference and current datasets
#'
#' Compares common features using missingness-rate differences, standardized
#' mean differences for numeric features, and a two-sample KS test when both
#' numeric samples contain at least two observations. Categorical features use
#' total variation distance over the union of observed levels.
#'
#' @param reference Baseline data frame.
#' @param current Incoming data frame.
#' @return A data frame with one diagnostic row per common feature.
#' @export
sl_compare <- function(reference, current) {
  sl_validate(reference)
  sl_validate(current)
  common <- intersect(names(reference), names(current))
  rows <- lapply(common, function(feature) {
    baseline <- reference[[feature]]
    incoming <- current[[feature]]
    numeric_pair <- is.numeric(baseline) && is.numeric(incoming) &&
      !is.factor(baseline) && !is.factor(incoming)
    missing_delta <- mean(is.na(incoming)) - mean(is.na(baseline))
    if (numeric_pair) {
      x <- baseline[is.finite(baseline)]
      y <- incoming[is.finite(incoming)]
      pooled_sd <- sqrt((stats::var(x) + stats::var(y)) / 2)
      mean_delta <- if (length(x) && length(y)) mean(y) - mean(x) else NA_real_
      standardized_delta <- if (length(x) && length(y) && is.finite(pooled_sd) && pooled_sd > 0) mean_delta / pooled_sd else NA_real_
      p_value <- if (length(x) >= 2L && length(y) >= 2L) {
        suppressWarnings(stats::ks.test(x, y)$p.value)
      } else NA_real_
      data.frame(feature, type = "numeric", missing_rate_delta = missing_delta,
        mean_delta = mean_delta, standardized_mean_delta = standardized_delta,
        categorical_tv = NA_real_, ks_p_value = p_value, stringsAsFactors = FALSE)
    } else {
      x <- as.character(baseline[!is.na(baseline)])
      y <- as.character(incoming[!is.na(incoming)])
      levels <- union(unique(x), unique(y))
      px <- if (length(x)) tabulate(match(x, levels), nbins = length(levels)) / length(x) else rep(0, length(levels))
      py <- if (length(y)) tabulate(match(y, levels), nbins = length(levels)) / length(y) else rep(0, length(levels))
      tv <- if (length(levels)) sum(abs(px - py)) / 2 else NA_real_
      data.frame(feature, type = "categorical", missing_rate_delta = missing_delta,
        mean_delta = NA_real_, standardized_mean_delta = NA_real_,
        categorical_tv = tv, ks_p_value = NA_real_, stringsAsFactors = FALSE)
    }
  })
  if (!length(rows)) {
    return(data.frame(feature = character(), type = character(), missing_rate_delta = numeric(),
      mean_delta = numeric(), standardized_mean_delta = numeric(), categorical_tv = numeric(), ks_p_value = numeric()))
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}
