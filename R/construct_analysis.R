#' Construct the results of the analysis
#'
#' @param blueprint The blueprint object
#' @param na.rm Whether to remove missing values
#' @param hclust.order Whether to order the correlation data based on the
#'   \code{\link[stats]{hclust}} algorithm.
#'
#' @return Uses the blueprint to construct the results of the statistical
#'   analysis.
#' @export
#'
#' @examples
#'
#' library(magrittr)
#' design_analysis(iris, 'cor') %>%
#'  add_settings() %>%
#'  add_variables('xvars', c('Sepal.Length', 'Sepal.Width')) %>%
#'  construct_analysis()
#'
construct_analysis <- function(blueprint, na.rm = TRUE, hclust.order = FALSE) {
    UseMethod("construct_analysis", blueprint)
}

#' @export
construct_analysis.gee_blueprint <- function(blueprint, na.rm = TRUE) {

    if (!requireNamespace('geepack'))
        stop('geepack is needed for this analysis, please install it',
             call. = FALSE)

    .is_not_empty(blueprint$yvars)
    .is_not_empty(blueprint$xvars)
    .check_vars_in_data(blueprint, c('cluster.id', 'yvars', 'xvars', 'covariates', 'interaction'))
    .check_xyvars_same_type(blueprint)
    .check_covars_in_xyvars(blueprint)

    id <- blueprint$id
    y <- blueprint$yvars
    x <- blueprint$xvars
    covars <- blueprint$covariates
    int <- blueprint$interaction

    if (length(c(y, x)) >= 100)
        stop(paste0('There are too many y and/or x variables to loop through, ',
                    'please split the construction up (see vignette for more details).'))

    data <- .regression_data_prep(
        data = blueprint$data,
        y = y,
        x = x,
        covars = covars,
        int = int,
        id = id
    )

    if (na.rm)
        data <- na.omit(data)

    if (length(int) > 1) {
        stop('Currently, only one interaction can be added.')
    } else if (length(int) == 1) {
        .check_int_in_covars(blueprint)
        int <- paste0('XtermValues:', int)
    }

    gee_formula <- reformulate(c('XtermValues', covars, int),
                               response = 'YtermValues')

    results <- data %>%
        dplyr::do(
            geepack::geeglm(
                gee_formula, data = ., id = id,
                corstr = blueprint$corstr,
                family = blueprint$family
            ) %>% {
                tidied.data <- broom::tidy(., conf.int = blueprint$conf.int,
                                           conf.level = blueprint$conf.level)
                nsize <- summary(.)$clusz
                data.frame(tidied.data, sample.total = sum(nsize),
                           sample.max = max(nsize), sample.min = min(nsize))
            }
        ) %>%
        dplyr::ungroup() %>%
        dplyr::tbl_df()
    blueprint <- .append_if_results_exist(blueprint, results)
    return(blueprint)
}

#' @export
construct_analysis.glm_blueprint <- function(blueprint, na.rm = TRUE) {

    .is_not_empty(blueprint$yvars)
    .is_not_empty(blueprint$xvars)
    .check_vars_in_data(blueprint)
    .check_xyvars_same_type(blueprint)
    .check_covars_in_xyvars(blueprint)

    y <- blueprint$yvars
    x <- blueprint$xvars
    covars <- blueprint$covariates
    int <- blueprint$interaction

    data <- .regression_data_prep(
        data = blueprint$data,
        y = y,
        x = x,
        covars = covars,
        int = int
    )

    if (na.rm)
        data <- na.omit(data)

    if (length(int) > 1) {
        stop('Currently, only one interaction can be added.')
    } else if (length(int) == 1) {
        .check_int_in_covars(blueprint)
        int <- paste0('XtermValues:', int)
    }

    glm_formula <- reformulate(c('XtermValues', covars, int),
                               response = 'YtermValues')

    results <-
        data %>%
        dplyr::do(
            glm(
                glm_formula, data = .,
                family = blueprint$family
            ) %>% {
                tidied.data <- broom::tidy(., conf.int = blueprint$conf.int,
                                           conf.level = blueprint$conf.level)
                data.frame(tidied.data, sample.size = nrow(.$model))
            }
        ) %>%
        dplyr::ungroup() %>%
        dplyr::tbl_df()

    blueprint <- .append_if_results_exist(blueprint, results)

    return(blueprint)
}

#' @export
construct_analysis.cor_blueprint <- function(blueprint, hclust.order = FALSE) {
    .is_not_empty(blueprint$xvars)
    .check_vars_in_data(blueprint, c('yvars', 'xvars'))
    .check_xyvars_same_type(blueprint)

    if (is.null(blueprint$yvars)) {
        y <- NULL
        x <- blueprint$data[blueprint$xvars]
    } else {
        y <- blueprint$data[blueprint$yvars]
        x <- blueprint$data[blueprint$xvars]
    }

    results <-
        cor(x, y,
            use = blueprint$obs.usage,
            method = blueprint$method) %>%
        .tidy_cor_results(blueprint = blueprint,
                          hclust.order = hclust.order) %>%
        dplyr::tbl_df()

    blueprint <- .append_if_results_exist(blueprint, results)

    return(blueprint)
}

