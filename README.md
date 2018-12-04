
<!-- README.md is generated from README.Rmd. Please edit that file -->
line4PPPsim
===========

[![Build Status](https://travis-ci.com/mgaldino/line4PPPsim.svg?branch=master)](https://travis-ci.com/mgaldino/line4PPPsim) [![HitCount](http://hits.dwyl.io/mgaldino/line4PPPsim.svg)](http://hits.dwyl.io/mgaldino/line4PPPsim)

This package allows the user to run Monte Carlo simulation on the fiscal impact of subway PPP. This version is focused only on line4 yellow. Future versions will include Rio de Janeiro's VLT.

Installation
------------

You can install the github version of line4PPPsim from [CRAN](https://CRAN.R-project.org) with:

``` r
library(devtools)
devtools::intall_github("mgaldino/line4PPPsim")
```

Example
-------

Como analisar impacto de ajuste de demanda com simulação.

``` r
library(line4PPPsim)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 3.5.1
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.5.1

resultado <- sim_sensibilidade_line4(n_sim = 100, num_years = 33, start_seed = 1, mu = 0, sd= .05, ajuste_inflacao = F, type="random_walk")
#> Warning: package 'bindrcpp' was built under R version 3.5.1

result_sim <- resultado[[1]]

result_sim %>% ggplot(aes(x=ano, y=diferenca, group = sim)) + 
  geom_line() + ylab("diferença entre realizado e previsto") +
  scale_y_continuous(labels = scales::comma_format(prefix = "R$ ",big.mark = ".", decimal.mark = ",")) +
  theme_bid()
```

<img src="man/figures/README-example-1.png" width="100%" />
