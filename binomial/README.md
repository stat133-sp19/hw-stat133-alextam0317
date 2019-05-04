## binomial


## Overview

'"binomial"' is a package that provides functions to simulate trials and success.

*'bin_choose()' is the number of success in n trials.
*'bin_probability()' is the probability given the number of success, trials and the probability of success.
*'bin_distribution()' gives you a nice binomial distribution table.
*'plot()' creates a graohic with the probability / cumulative probability of a series of trials.
*'bin_cumulative()' gives you a nice binomial distribution table with the cumulative probability column.
*'bin_variable()' prints a list with the number of trials and the probability of the class binvar
*'summary()' printing a summary list of the variable (including the number of trials, prob of success, mean, variance, mode, skewness and kurtosis) of the class binvar
*'bin_mean()' gives mean of the binomial distribution.
*'bin_variance()' gives variance of the binomial distribution.
*'bin_mode()' gives mode of the binomial distribution.
*'bin_skewness()' gives skewness of the binomial distribution.
*'bin_kurtosis()' gives kurtosis of the binomial distribution.

## Motivation
To develop convenient codes to compute binomial distribution and the related functions.

## Installation

Install the development version from GitHub via the package '"devtools"';

```r
# development version from GitHub:
#install.packages("devtools") 
# install "binomial" (without vignettes)
devtools::install_github("alextam0317/binomial")
# install "cointoss" (with vignettes)
devtools::install_github("alextam0317/binomial", build_vignettes = TRUE)
```

## Usage
``` {r}
bin_choose(n = 5, k = 2)
10

bin_probability(success = 2, trials = 5, prob = 0.5) = 0.3125

bin_distribution(trials = 5, prob = 0.5)

dis1 <- bin_distribution(trials = 5, prob = 0.5) 
plot(dis1)

bin_cumulative(trials = 5, prob = 0.5)

cum1 <- bin_cumulative(trials = 5, prob = 0.5)
plot(cum1)

bin_variable(trials = 10, p = 0.3)

bin1 <- bin_variable(trials = 10, p = 0.3)
binsum1 <- summary(bin1)
binsum1

bin_mean(10, 0.3)

bin_variance(10, 0.3)

bin_mode(10, 0.3)

bin_skewness(10, 0.3)

bin_kurtosis(10, 0.3)
