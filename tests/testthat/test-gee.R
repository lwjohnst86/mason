context("GEE output")

## TODO: Tests to maintain class after doing something else to it outside of mason (?)
## TODO: Use multiple datasets (state, dietox + respiratory + sitka89 (geepack))

# construct_table ---------------------------------------------------------

ds <- design_analysis(testdata, 'gee')
ds <- add_settings(ds, cluster.id = 'state.region')
test_that("(for gee) construct_analysis needs cluster.id and function for family", {
    ds_wrong <- add_settings(ds, 'state.region', family = 'gaussian')
    expect_error(contruct_analysis(ds_wrong))
    ds_wrong <- add_settings(ds, 'doesntexist')
    expect_error(contruct_analysis(ds_wrong))
})

test_that("(for gee) construct_analysis needs yvar or xvar", {
    ds_wrong <- add_variables(ds, 'xvars', c('Population', 'Murder'))
    expect_error(contruct_analysis(ds_wrong))

    ds_wrong <- add_variables(ds, 'yvars', c('Population', 'Murder'))
    expect_error(contruct_analysis(ds_wrong))
})

test_that("(for gee) construct_analysis yvar and xvar are in the data", {
    ds_wrong <- add_variables(ds, 'yvars', 'doesntexist')
    expect_error(construct_analysis(ds_wrong))

    ds_wrong <- add_variables(ds, 'xvars', 'doesntexist')
    expect_error(construct_analysis(ds_wrong))
})

test_that("(for gee) construct_analysis yvar and xvar are same data type", {
    ds_wrong <- add_variables(ds, 'yvars', c('Income', 'Rich'))
    expect_error(construct_analysis(ds_wrong))

    ds_wrong <- add_variables(ds, 'xvars', c('Income', 'Rich'))
    expect_error(construct_analysis(ds_wrong))
})

ds <- add_variables(ds, 'yvars', c('Income', 'Life.Exp'))
ds <- add_variables(ds, 'xvars', c('Population', 'Murder'))
test_that("(for gee) construct_analysis interaction is a covar", {
    ds <- add_variables(ds, 'covariates', c('Frost'))
    ds_wrong <- add_variables(ds, 'interaction', c('Area'))
    expect_error(construct_analysis(ds_wrong))
})

test_that("(for gee) construct_analysis there is only one interaction", {
    ds_wrong <- add_variables(ds, 'covariates', c('Frost', 'Area'))
    ds_wrong <- add_variables(ds_wrong, 'interaction', c('Frost', 'Area'))
    expect_error(construct_analysis(ds_wrong))
})

# Need to develop this test for when there are too many variables to loop through.
# test_that("(for gee) construct_analysis is not more than 100 y and x vars to run on", {
# })

# comparing to real results -----------------------------------------------

gee_function <- function(formula, data = testdata) {
    gee_data <- geepack::geeglm(
        formula,
        family = gaussian('identity'),
        data = data,
        corstr = 'ar1',
        id = state.region
    )
    nsize <- summary(gee_data)$clusz
    tidy_data <-
        broom::tidy(gee_data, conf.int = TRUE, conf.level = 0.95)
    data.frame(
        tidy_data,
        sample.total = sum(nsize),
        sample.max = max(nsize),
        sample.min = min(nsize)
    )
}
ds <- add_settings(ds, cluster.id = 'state.region', corstr = 'ar1')

test_that("(for gee) construct_analysis creates the right results (no covars)", {
    ds_lone <- construct_analysis(ds)$results[-1:-3]
    real_results <- rbind(
        gee_function(Income ~ Murder),
        gee_function(Income ~ Population),
        gee_function(Life.Exp ~ Murder),
        gee_function(Life.Exp ~ Population)
    )[-1]
    expect_equivalent(ds_lone, real_results)
})

ds <- add_variables(ds, 'covariates', c('Frost', 'Area'))
test_that("(for gee) construct_analysis creates the right results (with covars)", {
    ds_lone <- construct_analysis(ds)$results[-1:-3]
    real_results <- rbind(
        gee_function(Income ~ Murder + Frost + Area),
        gee_function(Income ~ Population + Frost + Area),
        gee_function(Life.Exp ~ Murder + Frost + Area),
        gee_function(Life.Exp ~ Population + Frost + Area)
    )[-1]
    expect_equivalent(ds_lone, real_results)
})

test_that("(for gee) construct_analysis creates the right results (with covars + int)", {
    ds <- add_variables(ds, 'interaction', c('Frost'))
    ds_lone <- construct_analysis(ds)$results[-1:-3]
    real_results <- rbind(
        gee_function(Income ~ Murder + Frost + Area + Murder:Frost),
        gee_function(Income ~ Population + Frost + Area + Population:Frost),
        gee_function(Life.Exp ~ Murder + Frost + Area + Murder:Frost),
        gee_function(Life.Exp ~ Population + Frost + Area + Population:Frost)
    )[-1]
    expect_equivalent(ds_lone, real_results)
})

# scrub and polish --------------------------------------------------------

ds <- construct_analysis(ds)
test_that("(for gee) scrub converts to tbl_df", {
    expect_is(scrub(ds), 'tbl_df')
})
