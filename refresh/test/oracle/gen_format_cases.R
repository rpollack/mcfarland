# Oracle for refresh/format.ts: what R's round() and readr's write_csv do to doubles.
# Run from the repository root: Rscript refresh/test/oracle/gen_format_cases.R
# Output: refresh/test/golden/format-cases.tsv with columns x, round3, round6 (each as readr writes it).
library(readr)
set.seed(20261126)
ties <- c(0.2345, 0.0005, 0.0015, 0.0025, 0.0035, 0.1235, 0.9995, 1.0005, 2.675, 1.005, 5.5e-4, 0.12345, 2.5, 0.5, 1.5, 0.0004999, 0.00050001,
          0.4445, 0.4455, 100.0005, 100.0015, 1234.5675, 0.3335, 0.0125, 0.0135, 0.5555, 7.0005, 9.9995, 33.3335, 66.6665)
ks <- seq(0, 3000, by = 7)
grid <- (ks + 0.5) / 1000
tiny <- c(1e-4, 1.5e-4, 9.99e-4, 1e-3, 1.234e-3, 1e-5, 5e-5, 1e-7, 3.3e-6, 0.00099999)
big <- c(1e5, 123456, 1e6, 12345678, 1234567.1, 100000.5, 999999.9995, 1e9, 123456789.123456)
rand <- c(
  round(runif(1500, 0, 1), sample(1:8, 1500, TRUE)),
  round(runif(800, 0, 100), sample(1:8, 800, TRUE)),
  round(rnorm(800, 0, 0.1), sample(3:9, 800, TRUE)),
  runif(500, 0, 1), 100 * runif(300), 9 * runif(200) / runif(200, 0.1, 1)
)
x <- unique(c(ties, grid, tiny, big, rand, -ties, -grid[1:200], -tiny, -rand[1:300], 0, 1, 100, 3))
x <- x[is.finite(x)]
fmt <- function(v) vapply(v, function(e) trimws(strsplit(format_csv(data.frame(a = e)), "\n")[[1]][2]), "")
out <- data.frame(x = fmt(x), round3 = fmt(round(x, 3)), round6 = fmt(round(x, 6)))
writeLines(c("x\tround3\tround6", paste(out$x, out$round3, out$round6, sep = "\t")), "refresh/test/golden/format-cases.tsv")
cat("wrote", nrow(out), "cases\n")
