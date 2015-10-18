#' Build a mason structure
#'
#'
#' @param data The data from the mason chain (last chain is
#'   \code{\link{lay_base}}.
#' @param ... Additional parameters appropriate for the \code{\link{design}}
#'
#' @return Outputs a list structure with the data, results, and additional
#'   design specifications for the analysis.
#' @export
#'
#' @examples
#' ds <- data.frame(state.region, state.x77)
#' ## GEE
#' design(ds, 'gee', family = gaussian, corstr = 'exchangeable') %>%
#'     lay_base(id = 'state.region', c('Income', 'Frost'), c('Population', 'Murder'), 'Life.Exp') %>%
#'     build()
#' ## Correlation
#' design(ds, 'cor') %>%
#'     lay_base(c('Income', 'Frost'), c('Population', 'Murder')) %>%
#'     build()
build <- function(data, ...) {
    UseMethod('build', data)
}

#' @export
#'
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

#' @export
#'
build.cor_df <- function(data) {
    if (is.null(data$y)) {
        yvars <- NULL
        xvars <- data$data
    } else {
        yvars <- data$data[data$y]
        xvars <- data$data[data$x]
    }

    data$results <-
        data$data %>%
        dplyr::do(cor(xvars, yvars,
                      use = data$use,
                      method = data$method) %>%
                      broom::tidy()) %>%
        dplyr::tbl_df()

    return(data)
}
