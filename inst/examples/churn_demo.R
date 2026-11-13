library(ShiftLensR)

reference <- data.frame(
  tenure_months = c(3, 12, 24, 36, 48, 60),
  monthly_charges = c(45, 55, 65, 70, 80, 85),
  contract = c("monthly", "monthly", "annual", "annual", "two_year", "two_year"),
  churn = c("yes", "no", "no", "no", "no", "no")
)

current <- data.frame(
  tenure_months = c(2, 8, 18, 30, 40, 52),
  monthly_charges = c(55, 65, 75, 80, 90, 95),
  contract = c("monthly", "monthly", "monthly", "annual", "annual", "two_year"),
  churn = c("yes", "yes", "no", "yes", "no", "no")
)

cat("Current data profile\n")
print(sl_profile(current))

cat("\nCurrent data quality\n")
print(sl_quality(current))

cat("\nReference versus current comparison\n")
print(sl_compare(reference, current))
