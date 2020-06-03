context("pls output")

skip_if_not_installed("pls")

testdata <- dplyr::mutate(testdata, Rich = as.factor(Rich))

# add_settings ------------------------------------------------------------

ds <- design(testdata, 'pls')
test_that("cv specs work in settings", {
    ds_right <- add_settings(ds, cv.data = TRUE)
    expect_match(class(attr(ds_right, 'specs')$cv.index), 'integer')

    ds_empty <- add_settings(ds, cv.data = FALSE)
    expect_equal(attr(ds_empty, 'specs')$cv.index, NULL)
})

# construct ---------------------------------------------------------------

ds <- design(testdata, 'pls')
ds <- add_variables(ds, 'xvars', c('Population', 'Income', 'Murder'))
ds <- add_variables(ds, 'yvars', c('Frost'))

test_that("test data is return dependent on cv.data", {
    ds_null <- add_settings(ds, cv.data = FALSE)
    expect_null(attr(construct(ds_null), 'specs')$results$test_data)

    ds_testdata <- add_settings(ds, cv.data = TRUE)
    expect_is(attr(construct(ds_testdata), 'specs')$results$test_data, 'data.frame')
})

ds <- add_settings(ds)
test_that("only numeric variables", {
    expect_error(add_variables(ds, 'xvars', c('Population', 'Income', 'Rich')))
})

# Scrub -------------------------------------------------------------------

ds <- construct(ds)
test_that("standard scrubbing returns a list and mvr object", {
    expect_type(scrub(ds), 'list')
    expect_is(scrub(ds), 'mvr')
})

test_that("default, loadings, corr, explained var output gives tibble with relevant results", {
    expect_is(scrub(ds, "default"), 'tbl_df')
    expect_equal(dim(scrub(ds, "default")), c(9, 5))
    expect_equivalent(
        names(scrub(ds, "default")),
        c(
            "xvariables",
            "components",
            "loadings",
            "scores.to.var.corr",
            "explained.variance"
        )
    )
    expect_is(scrub(ds, "explained_var"), 'tbl_df')
    expect_equal(dim(scrub(ds, "explained_var")), c(3, 2))
    expect_equivalent(
        names(scrub(ds, "explained_var")),
        c(
            "components",
            "explained.variance"
        )
    )
})

test_that("scores output gives tibble and the proper scores", {
    expect_is(scrub(ds, "scores"), 'tbl_df')
    expect_equal(dim(scrub(ds, "scores")), c(nrow(testdata)/2, 3))
    expect_equivalent(
        names(scrub(ds, "scores")),
        c(
            "scores.component.1",
            "scores.component.2",
            "scores.component.3"
        )
    )
})
