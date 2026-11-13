# ShiftLensR

ShiftLensR helps assess whether tabular data remains suitable for machine
learning workflows. Milestone 1 provides input validation, feature profiling,
basic quality checks, and reference-versus-current diagnostics.

## Install for development

```r
install.packages(c("devtools", "roxygen2", "testthat"))
devtools::document()
devtools::install()
```

## Quick start

```r
reference <- data.frame(age = c(24, 31, 42, 50), plan = c("basic", "pro", "basic", "pro"))
current <- data.frame(age = c(29, 36, 48, 61), plan = c("pro", "pro", "basic", "enterprise"))

sl_validate(reference)
sl_profile(reference)
summary(sl_profile(reference))
sl_quality(current)
sl_compare(reference, current)
```

Numeric comparisons include the mean change, standardized mean change, and a
two-sample Kolmogorov-Smirnov p-value where sample sizes permit. Categorical
comparisons report total variation distance. These are diagnostic signals and
should be interpreted with sample size and domain context.

## Development

Run package tests with `devtools::test()`. Run the full package check with
`devtools::check()`.
