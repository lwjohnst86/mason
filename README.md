
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Use masonry, build a (data) structure\! <img src="man/figures/logo.png" align="right" />

[![Travis-CI Build
Status](https://travis-ci.org/lwjohnst86/mason.svg?branch=master)](https://travis-ci.org/lwjohnst86/mason)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/lwjohnst86/mason?branch=master&svg=true)](https://ci.appveyor.com/project/lwjohnst86/mason)
[![Coverage
status](https://codecov.io/gh/lwjohnst86/mason/branch/master/graph/badge.svg)](https://codecov.io/github/lwjohnst86/mason?branch=master)
[![CRAN
Status](http://www.r-pkg.org/badges/version/mason)](https://cran.r-project.org/package=mason)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/mason)](http://www.r-pkg.org/pkg/mason)

Using a standard interface, create common data results structures, such
as from a linear regression or correlation. Design the analysis, add
settings and variables, construct the results, and lastly scrub and
polish it up.

One of the main goals of `mason` is to be able to easily implement other
analyses to this infrastructure. Since, I’d argue, most statistical
methods follow a similar pattern (what are the variables, what options
to use for the method, what to select from the results), this can be
easily encapsulated into a ‘blueprint -\> construction -\> scrubbing and
polishing’ workflow.

`mason` was designed to be best used with the `magrittr` `%>%` pipes,
though it doesn’t need to be. It was also designed to follow the [tidy
data
philosophy](https://CRAN.R-project.org/package=tidyr/vignettes/tidy-data.html),
specifically that everything should result in a data frame, within
limits. This makes it easier to do further analysis, visualization, and
inclusion into report formats. This flow was deliberately chosen so it
works well with `dplyr`, `tidyr`, `ggplot2`, and many other excellent
packages out there that help make analyses easier.

# Installation

The package can be installed from CRAN using:

    install.packages("mason")

For the development version, install using:

    # install.packages("remotes")
    remotes::install_github('lwjohnst86/mason')

# Typical usage

The typical usage for this package would flow like this:

``` r
library(mason)
design(iris, 'glm') %>%
    add_settings() %>%
    add_variables('yvars', c('Sepal.Length', 'Sepal.Width')) %>%
    add_variables('xvars', c('Petal.Length', 'Petal.Width')) %>%
    construct() %>%
    scrub() %>%
    polish_adjust_pvalue()
#> # A tibble: 8 x 11
#>   Yterms   Xterms   term   estimate std.error statistic   p.value conf.low
#>   <chr>    <chr>    <chr>     <dbl>     <dbl>     <dbl>     <dbl>    <dbl>
#> 1 Sepal.L… Petal.L… (Inte…    4.31     0.0784     54.9  2.43e-100    4.15 
#> 2 Sepal.L… Petal.L… <-Xte…    0.409    0.0189     21.6  1.04e- 47    0.372
#> 3 Sepal.L… Petal.W… (Inte…    4.78     0.0729     65.5  3.34e-111    4.63 
#> 4 Sepal.L… Petal.W… <-Xte…    0.889    0.0514     17.3  2.33e- 37    0.788
#> 5 Sepal.W… Petal.L… (Inte…    3.45     0.0761     45.4  9.02e- 89    3.31 
#> 6 Sepal.W… Petal.L… <-Xte…   -0.106    0.0183     -5.77 4.51e-  8   -0.142
#> 7 Sepal.W… Petal.W… (Inte…    3.31     0.0621     53.3  1.84e- 98    3.19 
#> 8 Sepal.W… Petal.W… <-Xte…   -0.209    0.0437     -4.79 4.07e-  6   -0.295
#> # ... with 3 more variables: conf.high <dbl>, sample.size <int>,
#> #   adj.p.value <dbl>
```

Depending on the statistical method being used, each function may have
slightly different arguments.

# Problems?

If there are problems, [create an
issue](https://github.com/lwjohnst86/mason/issues) and let me know what
the problem is\!

# Contributing a statistical method

1.  Add the method to `design`
2.  Add a new function to the S3 method `add_settings` following the
    naming convention `add_settings.statmethod_bp` and include the
    appropriate settings to the statistical method.
3.  If needed, add another option to the `type` argument in the
    `add_variables` function.
4.  Like the `add_settings` instructions above, do the same for the
    `construct` and `scrub` S3 method.
5.  If needed, add another `polish_` type function.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
