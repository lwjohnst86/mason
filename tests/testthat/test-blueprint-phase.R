context("blueprint phase (design, settings, variables)")

# design ---------------------------------------------------------

test_that("design is of correct class and is a list", {
    ds <- design(testdata, 'gee')
    expect_is(ds, 'gee_blueprint')
    expect_output(str(ds), 'List of 2')

    ds <- design(testdata, 'cor')
    expect_is(ds, 'cor_blueprint')
    expect_output(str(ds), 'List of 2')

    ds <- design(testdata, 'glm')
    expect_is(ds, 'glm_blueprint')
    expect_output(str(ds), 'List of 2')
})

# add_setting -------------------------------------------------------------

test_that("add_settings is correct class and is a list", {
    ds <- design(testdata, 'gee')
    ds <- add_settings(ds, cluster.id = 'state.region')
    expect_is(ds, 'gee_blueprint')
    expect_output(str(ds), 'List of 7')

    ds <- design(testdata, 'cor')
    ds <- add_settings(ds)
    expect_is(ds, 'cor_blueprint')
    expect_output(str(ds), 'List of 5')
})

# add_variable ------------------------------------------------------------

test_that("add_variables is correct class and has correct number of list items", {
    ds <- design(testdata, 'gee')
    ds <- add_variables(ds, 'yvars', c('Income', 'Frost'))
    ds <- add_variables(ds, 'xvars', c('Population', 'Murder'))
    ds <- add_variables(ds, 'covariates', c('Life.Exp'))
    expect_is(ds, 'gee_blueprint')
    expect_output(str(ds), 'List of 5')

    ds <- design(testdata, 'cor')
    ds <- add_variables(ds, 'yvars', c('Income', 'Frost'))
    ds <- add_variables(ds, 'xvars', c('Population', 'Murder'))
    expect_is(ds, 'cor_blueprint')
    expect_output(str(ds), 'List of 4')
})

test_that("add_variables asserting works", {
    ds <- design(testdata, 'cor')
    ds <- add_variables(ds, 'yvars', c('Income', 'Frost'))
    expect_message(add_variables(ds, 'yvars', 'Income'))
    expect_error(add_variables(iris))
    ds <- add_variables(ds, 'xvars', c('Area'))
    expect_message(add_variables(ds, 'xvars', 'Population'))
})

test_that("blueprint list is updated when another add_* is used", {
    ds <- design(testdata, 'gee')
    ds <- add_settings(ds, cluster.id = 'state.region')
    ds <- add_settings(ds, cluster.id = 'state.region', corstr = 'ar1')
    expect_is(ds, 'gee_blueprint')
    expect_output(str(ds), 'List of 7')

    ds <- add_variables(ds, 'yvars', c('Income', 'Area'))
    expect_is(ds, 'gee_blueprint')
    expect_output(str(ds), 'List of 8')
})