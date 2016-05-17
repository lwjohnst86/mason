<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/lwjohnst86/mason.svg?branch=master)](https://travis-ci.org/lwjohnst86/mason)

Use masonry, build a (data) structure!
======================================

Using a standard interface, create common data results structures, such as from a linear regression or correlation. Design the analysis, add settings and variables, construct the results, and lastly scrub and polish it up.

One of the main goals of `mason` is to be able to easily implement other analyses to this infrastructure. Since, I'd argue, most statistical methods follow a similar pattern (what are the variables, what options to use for the method, what to select from the results), this can be easily encapsulated into a 'blueprint -&gt; construction -&gt; scrubbing and polishing' workflow.

`mason` was designed to be best used with the `magrittr` `%>%` pipes, though it doesn't need to be. It was also designed to follow the [tidy data philosophy](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html), specifically that everything should result in a data frame, within limits. This makes it easier to do further analysis, visualization, and inclusion into report formats.

This project is still in development.

Installation
============

So far this package can only be installed from GitHub, using:

    devtools::install_github('lwjohnst86/mason')
    ## Load it by:
    library(mason)

Problems?
=========

If there are problems, [create an issue](https://github.com/lwjohnst86/mason/issues) and let me know what the problem is!

Typical usage
=============

The typical usage for this package would flow like this:

``` r
library(magrittr)
library(mason)
design_analysis(iris, 'glm') %>%
    add_settings() %>%
    add_variables('yvars', c('Sepal.Length', 'Sepal.Width')) %>%
    add_variables('xvars', c('Petal.Length', 'Petal.Width')) %>%
    construct_analysis() %>%
    scrub() %>%
    polish_adjust_pvalue()
#> Source: local data frame [8 x 11]
#> 
#>         Yterms       Xterms         term   estimate  std.error statistic
#>          (chr)        (chr)        (chr)      (dbl)      (dbl)     (dbl)
#> 1 Sepal.Length Petal.Length  (Intercept)  4.3066034 0.07838896 54.938900
#> 2 Sepal.Length Petal.Length Petal.Length  0.4089223 0.01889134 21.646019
#> 3 Sepal.Length  Petal.Width  (Intercept)  4.7776294 0.07293476 65.505517
#> 4 Sepal.Length  Petal.Width  Petal.Width  0.8885803 0.05137355 17.296454
#> 5  Sepal.Width Petal.Length  (Intercept)  3.4548745 0.07609540 45.401882
#> 6  Sepal.Width Petal.Length Petal.Length -0.1057853 0.01833860 -5.768449
#> 7  Sepal.Width  Petal.Width  (Intercept)  3.3084256 0.06209746 53.277950
#> 8  Sepal.Width  Petal.Width  Petal.Width -0.2093598 0.04374001 -4.786461
#> Variables not shown: p.value (dbl), conf.low (dbl), conf.high (dbl),
#>   sample.size (int), adj.p.value (dbl)
```

Depending on the statistical method being used, each function may have slightly different arguments.

Contributing a statistical method
=================================

1.  Add the method to `design_analysis`
2.  Add a new function to the S3 method `add_settings` following the naming convention `add_settings.statmethod_blueprint` and include the appropriate settings to the statistical method.
3.  If needed, add another option to the `type` argument in the `add_variables` function.
4.  Like the `add_settings` instructions above, do the same for the `construct_analysis` and `scrub` S3 method.
5.  If needed, add another `polish_` type function.
