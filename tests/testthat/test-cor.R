context("Correlation output")


# construct_table ---------------------------------------------------------

ds <- design_analysis(testdata, 'cor')
ds <- add_settings(ds)
test_that("(cor) needs at least xvar, maybe yvar", {
    ds_wrong <- add_variables(ds, 'yvars', 'Income')
    expect_error(construct_analysis(ds_wrong))
})
test_that("(cor) variables are in data", {
    ds_wrong <- add_variables(ds, 'xvars', c('Income', 'Population', 'doesntexist'))
    expect_error(construct_analysis(ds_wrong))
})

test_that("(cor) variables are the same type", {
    ds_wrong <- add_variables(ds, 'xvars', c('Income', 'Rich'))
    expect_error(construct_analysis(ds_wrong))
})

# comparing to real results -----------------------------------------------

test_that("(cor) results compare to real results", {
    cor_function <- function(x, y = NULL) {
        cor_data <- cor(x, y,
                        use = 'complete.obs',
                        method = 'pearson')
        if (is.null(y))
            cor_data[upper.tri(cor_data)] <- NA
        cor_data
    }

    ds <- design_analysis(testdata, 'cor')
    ds <- add_settings(ds)
    vars <- c('Income', 'Population', 'Frost', 'Area')
    ds <- add_variables(ds, 'xvars', vars)
    test_results <- construct_analysis(ds)$results
    real_results <- cor_function(testdata[vars])
    expect_equal(test_results[-1], real_results)

    yvars <- c('Murder', 'HS.Grad', 'Illiteracy')
    ds <- add_variables(ds, 'yvars', yvars)
    test_results <- construct_analysis(ds)$results
    real_results <- cor_function(testdata[vars], testdata[yvars])
    expect_equal(test_results[-1], real_results)
})