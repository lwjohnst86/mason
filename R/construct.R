#' Construct the results of the analysis
#'
#' @param data The blueprint data object.
#' @param na.rm Whether to remove missing values.
#' @param ... Additional args.
#'
#' @return Uses the blueprint to construct the results of the statistical
#'   analysis. Outputs a [tibble][tibble::tibble-package].
#' @export
#'
#' @examples
#'
#' design(iris, 'cor') %>%
#'  add_settings() %>%
#'  add_variables('xvars', c('Sepal.Length', 'Sepal.Width')) %>%
#'  construct()
#'
#' design(iris, 't.test') %>%
#'  add_settings() %>%
#'  add_variables('yvars', c('Sepal.Length', 'Sepal.Width')) %>%
#'  add_variables('xvars', c('Petal.Length', 'Petal.Width')) %>%
#'  construct()
#'
#' design(iris, 'glm') %>%
#'  add_settings() %>%
#'  add_variables('yvars', c('Sepal.Length', 'Sepal.Width')) %>%
#'  add_variables('xvars', c('Petal.Length', 'Petal.Width')) %>%
#'  construct()
#'
#' design(iris, 'gee') %>%
#'  add_settings('Species') %>%
#'  add_variables('yvars', c('Sepal.Length', 'Sepal.Width')) %>%
#'  add_variables('xvars', c('Petal.Length', 'Petal.Width')) %>%
#'  construct()
#'
#' design(iris, 'pls') %>%
#'  add_settings() %>%
#'  add_variables('yvars', c('Sepal.Length', 'Sepal.Width')) %>%
#'  add_variables('xvars', c('Petal.Length', 'Petal.Width')) %>%
#'  construct()
#'
construct <- function(data, ..., na.rm = TRUE) {
    UseMethod("construct", data)
}

#' @export
construct.gee_bp <- function(data, na.rm = TRUE, ...) {

    if (!requireNamespace('geepack'))
        stop('geepack is needed for this analysis, please install it',
             call. = FALSE)

    specs <- attributes(data)$specs
    specs_integrity(data, specs)

    f <- function(data, specs, form) {
        # Have to add these because I think geeglm doesn't reference them
        # explicitly inside its function
        glm <- stats::glm
        model.frame <- stats::model.frame
        id <- data[['id']]

        mod <- geepack::geeglm(
            form,
            data = data,
            id = id,
            corstr = specs$corstr,
            family = specs$family
        )
        tidied <-
            broom::tidy(mod,
                        conf.int = specs$conf.int,
                        conf.level = specs$conf.level)
        nsize <- summary(mod)$clusz

        data.frame(
            Yterms = all.vars(form)[1],
            Xterms = all.vars(form)[2],
            tidied,
            sample.total = sum(nsize),
            sample.max = max(nsize),
            sample.min = min(nsize),
            stringsAsFactors = FALSE
        )
    }
    form <- regression_formula(specs)

    construction_base(data = data, specs = specs, tool = f,
                       formulas = form$formulas, na.rm = na.rm)
}

#' @export
construct.glm_bp <- function(data, na.rm = TRUE, ...) {
    specs <- attributes(data)$specs
    specs_integrity(data, specs)

    tool <- function(data, specs, form) {
        mod <- stats::glm(form,
                          data = data,
                          family = specs$family)
        tidied <-
            broom::tidy(mod,
                        conf.int = specs$conf.int,
                        conf.level = specs$conf.level)
        data.frame(
            Yterms = all.vars(form)[1],
            Xterms = all.vars(form)[2],
            tidied,
            sample.size = nrow(mod$model),
            stringsAsFactors = FALSE
        )
    }
    form <- regression_formula(specs)

    construction_base(data = data, specs = specs, tool = tool,
                      formulas = form$formulas, na.rm = na.rm)
}

#' @export
construct.cor_bp <- function(data, ...) {

    specs <- attributes(data)$specs
    specs_integrity(data, specs, stat = 'cor')

    if (is.null(specs$vars$yvars)) {
        y <- NULL
        x <- data[specs$vars$xvars]
    } else {
        y <- data[specs$vars$yvars]
        x <- data[specs$vars$xvars]
    }

    results <-
        stats::cor(x, y,
            use = specs$obs.usage,
            method = specs$method)

    if (specs$hclust.order & is.null(specs$vars$yvar)) {
        # Taken from
        # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
        ordering <- stats::hclust(stats::as.dist((1 - results) / 2))
        results <- results[ordering$order, ordering$order]
    }

    if (is.null(specs$vars$yvar))
        results[upper.tri(results)] <- NA

    results <- dplyr::as_tibble(results, rownames = "Variables")

    type <- grep('bp', class(data), value = TRUE)
    output <- append_results(data, specs, results, type)

    return(output)
}

#' @export
construct.t.test_bp <- function(data, na.rm = TRUE, ...) {

    specs <- attributes(data)$specs
    specs_integrity(data, specs)

    tool <- function(data, specs, form) {
        y <- form[1]
        x <- form[2]
        results <- stats::t.test(x = data[[x]],
                                 y = data[[y]],
                                 paired = specs$paired)
        results <- broom::tidy(results)
        data.frame(
            Yterms = y,
            Xterms = x,
            results,
            stringsAsFactors = FALSE
        )
    }

    form <- regression_formula(specs)
    # convert so that each x-y pair is a column
    form <- dplyr::as_tibble(t(form$variables), .name_repair = make.names)
    construction_base(data = data, specs = specs, tool = tool,
                      formulas = form, na.rm = na.rm)
}

#' @export
construct.pls_bp <- function(data, ...) {

    if (!requireNamespace('pls', quietly = TRUE)) {
        stop('Please install the pls package to run this analysis.')
    }

    specs <- attributes(data)$specs
    specs_integrity(data, specs, stat = 'pls')

    if (!is.null(specs$cv.index)) {
        d <- data[specs$cv.index,]
        test <- data[-specs$cv.index,]
    } else if (is.null(specs$cv.index)) {
        d <- data
        test <- NULL
    }

    if (is.null(specs$ncomp))
        specs$ncomp <- length(specs$vars$xvars)

    # TODO: This or use data in the pls instead?
    Y <- as.matrix(d[, specs$vars$yvars])
    X <- as.matrix(d[, specs$vars$xvars])
    results <- pls::plsr(Y ~ X, scale = specs$scale, ncomp = specs$ncomp)

    if (!is.null(test))
        results$test_data <- test

    type <- grep('bp', class(data), value = TRUE)
    output <- make_blueprint(data, results = results, type = type)
    return(output)
}