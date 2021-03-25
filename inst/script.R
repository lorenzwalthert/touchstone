library(touchstone)

benchmark_run_ref(
  expr_before_benchmark = c("print(4)"),
  expr1 = "tail(mtcars)",
  n = 2
)

benchmark_run_ref(
  expr_before_benchmark = c("print(4)"),
  expr2 = "head(mtcars)",
  n = 2
)

benchmarks_analyse()
