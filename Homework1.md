STAT 4100 Homework 1
================
Ryan Lynch
2026-01-23

Problem 3, part b

``` r
f <- function(u) {
  u^4 / (1+u^6)
}
```

``` r
num_int <- integrate(f, lower = 0, upper = 1)$value
```

``` r
x_vals <- seq(1,5, by = 0.1)
N_vals <- floor(10^x_vals)

set.seed(1)
E_vals <- numeric(length(N_vals))
```

``` r
for (i in seq_along(N_vals)){
  N <- N_vals[i]
  U <- runif(N)
  V <- runif(N)
  E_vals[i] <- mean(V <= f(U))
}
```

``` r
plot(
  N_vals, E_vals,
  log = "x",
  type = "l",
  lwd = 2,
  col = "blue",
  xlab = "N (log scale)",
  ylab = "Monte Carlo Estimate E(N)",
  main = "Monte Carlo Estimation of I"
)
abline(h = num_int, col = "red", lwd = 2, lty = 2)
legend(
  "topright",
  legend = c("Monte Carlo estimate E(N)", "Numerical Integration Value"),
  col = c("blue", "red"),
  lty = c(1, 2),
  lwd = 2
)
```

<img src="Homework1_files/figure-gfm/unnamed-chunk-5-1.png" width="60%" style="display: block; margin: auto;" />
We can see from the graph that, as N increases, E(N) begins to fluctuate
less and less and start to converge to the value obtained from using
numerical integration. The graph above also displays a slow convergence
rate from the Monte Carlo simulation, displaying why it is considered
slow and inefficient compared to numerical methods for low-dimensions
situations.


Problem 4, part b

``` r
N <- 1e5

set.seed(1)
x1 <- runif(N)
x2 <- runif(N)
x3 <- runif(N)

T <- pmax(x1, x2, x3)
```

``` r
hist(
  T,
  breaks = 50,
  probability = TRUE,   
  col = "lightgray",
  border = "white",
  main = "Histogram of T with Theoretical PDF",
  xlab = "T (hours after 6PM)"
)
curve(
  3 * x^2,
  from = 0,
  to = 1,
  add = TRUE,
  col = "red",
  lwd = 2
)
legend(
  "topleft",
  legend = c("Simulated histogram", "Theoretical PDF: 3t^2"),
  col = c("lightgray", "red"),
  lwd = c(10, 2),
  bty = "n"
)
```

<img src="Homework1_files/figure-gfm/unnamed-chunk-7-1.png" width="60%" style="display: block; margin: auto;" />
To make sure the histogram was normalized properly, I had to set
probability = TRUE. Imposing the theoretical pdf from part (a) onto the
normalized historgram, we can see that the histogram closely matches the
theoretical pdf. Another natural conclusion we can make when looking at
this graph is that we see that the density increases as we get closer to
7 pm meaning the chance that everyone has arrived is likelier, which
intuitively makes sense.

