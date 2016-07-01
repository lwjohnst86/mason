context("t-test output")

# construct_table ---------------------------------------------------------

ds <- design(testdata, 't.test')
ds <- add_settings(ds)
ds <- add_variables(ds, 'yvars', 'Income')
test_that("(t.test) variables are in data", {
    ds_wrong <- add_variables(ds, 'xvars', c('Population', 'doesntexist'))
    expect_error(construct(ds_wrong))
})

test_that("(t.test) variables are the same type", {
    ds_wrong <- add_variables(ds, 'xvars', c('Income', 'Rich'))
    expect_error(construct(ds_wrong))
})

# comparing to real results -----------------------------------------------

ds <- design(testdata, 't.test')
ds <- add_settings(ds)
vars <- c('Income', 'Population', 'Frost', 'Area')
ds <- add_variables(ds, 'yvars', vars[1:2])
ds <- add_variables(ds, 'xvars', vars[3:4])

test_that("(t.test) results compare to real results", {
    test_results <- construct(ds)$results
    test_results <- dplyr::rename(test_results, estimate1 = ymean, estimate2 = xmean)
    real_results <- rbind(
        broom::tidy(t.test(testdata$Income, testdata$Area)),
        broom::tidy(t.test(testdata$Income, testdata$Frost)),
        broom::tidy(t.test(testdata$Population, testdata$Area)),
        broom::tidy(t.test(testdata$Population, testdata$Frost))
    ) %>%
        dplyr::tbl_df()
    expect_equal(test_results[c(5, 2, 4, 6:10)], real_results)
})

# scrub and polish --------------------------------------------------------

ds <- construct(ds)
test_that("(for cor) scrub converts to tbl_df", {
    expect_is(scrub(ds), 'tbl_df')
})