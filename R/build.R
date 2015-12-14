#' Build a mason structure
#'
#'
#' @param data The data from the mason chain (previous chain is
#'   \code{\link{lay_base}}.
#' @param ... Additional parameters appropriate for the \code{\link{design}}
#'
#' @return Outputs a list structure with the data, results, and additional
#'   design specifications for the analysis.
#' @export
build <- function(data, ...) {
    UseMethod('build', data)
}

#' @param conf.int Whether to include the confidence interval
#' @param conf.level Range for the confidence interval
#' @rdname build
#' @export
#' @examples
#' ds <- data.frame(state.region, state.x77)
#' ## GEE
#' design(ds, 'gee', family = gaussian, corstr = 'exchangeable') %>%
#'     lay_base(id = 'state.region', c('Income', 'Frost'), c('Population', 'Murder'), 'Life.Exp') %>%
#'     build()
build.gee_df <- function(data, conf.int = TRUE, conf.level = 0.95) {
    gee_formula <- reformulate(c(data$x, data$covars, data$intvar),
                               response = paste(data$y))
    data$results <-
        data$data %>%
        dplyr::group_by(Yterms, Xterms) %>%
        dplyr::do(
            geepack::geeglm(
                gee_formula, data = ., id = id,
                corstr = paste(data$corstr),
                family = data$family
            ) %>%
                broom::tidy(., conf.int = conf.int, conf.level = conf.level)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::tbl_df()

    return(data)
}

#' @rdname build
#' @export
#' @examples
#' ds <- data.frame(state.region, state.x77)
#' ## Correlation
#' design(ds, 'cor') %>%
#'     lay_base(c('Income', 'Frost'), c('Population', 'Murder')) %>%
#'     build()
build.cor_df <- function(data) {
    if (is.null(data$y)) {
        yvars <- NULL
        xvars <- data$data
        .prep_cor_tidy <- function(cor_data) {
            cor_data[upper.tri(cor_data)] <- NA
            return(cor_data)
        }
    } else {
        yvars <- data$data[data$y]
        xvars <- data$data[data$x]
        .prep_cor_tidy <- function(cor_data) {
            return(cor_data)
        }
    }

    if (is.null(yvars)) {
        .cor_order <- function(cor_data) {
            # Taken from
            # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
            ord <-
                as.dist((1 - cor_data) / 2) %>%
                hclust()
            cor_data[ord$order, ord$order]
        }
    } else {
        .cor_order <- function(cor_data)
            cor_data
    }

    data$results <-
        data$data %>%
        dplyr::do(cor(xvars, yvars,
                      use = data$use,
                      method = data$method) %>%
                      .cor_order() %>%
                      .prep_cor_tidy() %>%
                      broom::tidy()) %>%
        dplyr::tbl_df()

    return(data)
}

#' @param conf.int Whether to include the confidence interval
#' @param conf.level Range for the confidence interval
#' @param ... Additional arguments to \code{\link[stats]{lm}}
#' @rdname build
#' @export
#' @examples
#'
#'## lm
#'ds <- data.frame(state.region, state.x77)
#'design(ds, 'lm') %>%
#'    lay_base(c('Income', 'Frost'), c('Population', 'Murder'), covars = 'Life.Exp') %>%
#'    build()
#'design(ds, 'lm') %>%
#'    lay_base(c('Income', 'Frost'), c('Population', 'Murder'), covars = 'Life.Exp',
#'             intvar = 'Life.Exp') %>%
#'    build()
#'
build.lm_df <- function(data, conf.int = TRUE, conf.level = 0.95, ...) {
    lm_formula <- reformulate(c(data$x, data$covars, data$intvar),
                               response = paste(data$y))
    data$results <-
        data$data %>%
        dplyr::group_by(Yterms, Xterms) %>%
        dplyr::do(
            lm(lm_formula, data = ., ...) %>%
                broom::tidy(., conf.int = conf.int, conf.level = conf.level)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::tbl_df()

    return(data)
}