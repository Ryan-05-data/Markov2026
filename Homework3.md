Homework 3
================
Ryan Lynch
2026-02-06

Problem 1, part d

``` r
# Define densities
f <- function(x) {
  (1/3) * x * (1 + x) * exp(-x)
}

g <- function(x, a) {
  a^2 * x * exp(-a * x)
}

# Optimal a*
a_star <- sqrt(3) - 1

# c(a*)
c_a_star <- exp(-a_star) / (3 * a_star^2 * (1 - a_star))

# Grid of x values
x <- seq(0, 10, length.out = 1000)

# Plot f(x)
plot(x, f(x),
     type = "l",
     lwd = 2,
     col = "blue",
     xlab = "x",
     ylab = "Density",
     main = expression(
       "Target density " ~ f(x) ~
       " and envelope " ~ c(a[star]) * g[a[star]](x)
     )) +

# Add scaled proposal
lines(x, c_a_star * g(x, a_star),
      lwd = 2,
      col = "red")
```

    ## integer(0)

``` r
# Legend
legend("topright",
       legend = c(expression(f(x)),
                  expression(c(a[star]) * g[a[star]](x))),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")
```

<img src="Homework3_files/figure-gfm/unnamed-chunk-1-1.png" width="60%" style="display: block; margin: auto;" />
Problem 2, part f

``` r
set.seed(1)

P <- matrix(c(
  0.9, 0.1, 0,
  0, 0.875, 0.125, 
  0.4, 0, 0.6
), nrow = 3, byrow = TRUE)

n_steps <- 10000
states <- integer(n_steps)


states[1] <- 1

for (t in 2:n_steps){
  states[t] <- sample(1:3, size = 1, prob = P[states[t-1], ])
}

fraction_time_G <- mean(states == 1)
fraction_time_G
```

    ## [1] 0.4866

Problem 4, part c

``` r
P <- matrix(c(1/2, 1/2, 0,   0,   0,   0,
              0,   1/2, 1/2, 0,   0,   0,
              1/3, 0,   1/3, 1/3, 0,   0,
              0,   0,   0,   1/2, 1/2, 0,
              0,   0,   0,   0,   0,   1,
              0,   0,   0,   0,   1,   0), 
            nrow = 6, byrow = TRUE)

n_trials <- 10000
n_steps <- 5
start_state <- 1
target_state <- 4
```

``` r
count_target <- 0

set.seed(123) # For reproducibility
for (i in 1:n_trials) {
  current_state <- start_state
  for (t in 1:n_steps) {
    # Sample next state based on current row of P
    current_state <- sample(1:6, size = 1, prob = P[current_state, ])
  }
  # Check if final state is the target
  if (current_state == target_state) {
    count_target <- count_target + 1
  }
}
```

``` r
fraction <- count_target / n_trials

# Theoretical calculation using matrix power
library(expm)
theoretical <- (P %^% 5)[1, 4]

cat("Simulated Fraction:", fraction, "\n")
```

    ## Simulated Fraction: 0.1768

``` r
cat("Theoretical Prob:", theoretical, "\n")
```

    ## Theoretical Prob: 0.1759259
