STAT 4100 Homework 4
================
Ryan Lynch
2026-02-26

Problem 1, part c

``` r
set.seed(1)

p <- 0.35
q <- 0.40
s <- 0.25

simulate_game <- function(start = 10) {
  money <- start
  
  repeat{
    if (money == 0) return (0)
    u <- runif(1)
    if (u < s) {
      return(money)
    } else if (u < s + p) {
      money <- money + 1
    } else {
      money <- money - 1
    }
  }
}
N <- 100000
results <- replicate(N, simulate_game(10))
mean(results)
```

    ## [1] 9.79583

Now, we compare it to the theoretical value

``` r
i <- 10
theoretical <- i 
theoretical
```

    ## [1] 10

The simulation value is relatively close to the theoretical value.
