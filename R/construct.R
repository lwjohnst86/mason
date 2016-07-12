#' Construct the results of the analysis
#'
#' @param data The blueprint data object.
#' @param na.rm Whether to remove missing values.
#' @param ... Additional args.
#'
#' @return Uses the blueprint to construct the results of the statistical
#'   analysis.
#' @export
#'
#' @examples
#'
#' library(magrittr)
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

    if (length(c(specs$vars$yvars, specs$vars$xvars)) >= 100)
        stop(
            'There are too many y and/or x variables to loop through, ',
            'please split the construction up (see vignette for more details).'
        )

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
            tidied,
            sample.total = sum(nsize),
            sample.max = max(nsize),
            sample.min = min(nsize)
        )
    }
    form <- regression_formula(specs)
    tool <- lazyeval::interp("f(., specs = specs, form = form)",
                                          f = f,
                                          specs = specs,
                                          form = form)

    construction_base(data = data, specs = specs, tool = tool, na.rm = na.rm)
}

#' @export
construct.glm_bp <- function(data, na.rm = TRUE, ...) {
    specs <- attributes(data)$specs
    specs_integrity(data, specs)

    f <- function(data, specs, form) {
        mod <- stats::glm(form,
                          data = data,
                          family = specs$family)
        tidied <-
            broom::tidy(mod,
                        conf.int = specs$conf.int,
                        conf.level = specs$conf.level)
        data.frame(tidied,
                   sample.size = nrow(mod$model))
    }
    form <- regression_formula(specs)
    tool <- lazyeval::interp("f(., specs = specs, form = form)",
                                          f = f,
                                          specs = specs,
                                          form = form)

    construction_base(data = data, specs = specs, tool = tool, na.rm = na.rm)
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

    results <- broom::tidy(results) %>%
        dplyr::rename_('Variables' = '.rownames') %>%
        dplyr::tbl_df()

    type <- grep('bp', class(data), value = TRUE)
    output <- append_results(data, specs, results, type)

    return(output)
}

#' @export
construct.t.test_bp <- function(data, na.rm = TRUE, ...) {

    specs <- attributes(data)$specs
    specs_integrity(data, specs)

    f <- function(data, specs) {
        broom::tidy(stats::t.test(data$YtermValues, data$XtermValues,
                                  paired = specs$paired))
    }
    tool <- lazyeval::interp("f(., specs = specs)",
                             f = f,
                             specs = specs)

    construction_base(data = data, specs = specs, tool = tool, na.rm = na.rm)
}

    # data.prep <- data
    # Y <- as.matrix(dplyr::select(data.prep, -matches('^pct_tg\\d+')))
    # X <- as.matrix(dplyr::select(data.prep, matches('^pct_tg\\d+')))
    # fit <- pls::plsr(Y ~ X, scale = TRUE, ncomp = 2)
    # return(fit)