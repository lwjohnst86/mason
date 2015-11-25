#' Lay the foundation for the mason project
#'
#' Lay the base of the masonry project to eventually be built by getting the
#' data ready to be analyzed.
#' @param data Output from the \code{\link{design}} phase.
#' @param ... Additional parameters.
#'
#' @return Creates a prepared list of the data and specifications for further
#'   analysis
#' @export
lay_base <- function(data, ...) {
    UseMethod('lay_base', data)
}

#' @param id ID variable used for \code{\link[geepack]{geeglm}} analysis.
#' @param y The outcome/dependent variable(s) for regression type designs, or a
#'   list of variables to compare with \code{x} for correlation designs.
#' @param x The predictor/exposure/independent variable(s) for regression type
#'   designs, or a list of variables to compare with \code{y} for correlation
#'   designs (or just a dataframe).
#' @param covars Covariates to control for in regression designs.
#' @param intvar Interaction variable (only one) to include in regression
#'   designs.
#' @param na.rm Remove missing values.
#' @rdname lay_base
#' @export
#' @examples
#'
#' ds <- data.frame(state.region, state.x77)
#' ## GEE
#' design(ds, 'gee', family = gaussian, corstr = 'exchangeable') %>%
#'     lay_base(id = 'state.region', c('Income', 'Frost'), c('Population', 'Murder'), 'Life.Exp')
lay_base.gee_df <- function(data,
                            id,
                            y,
                            x,
                            covars = NULL,
                            intvar = NULL,
                            na.rm = TRUE) {

    data$data <-
        data$data %>%
        dplyr::select_(.dots = c(id, y, x, covars, intvar)) %>%
        tidyr::gather_('Yterms', 'Yterm', y) %>%
        tidyr::gather_('Xterms', 'Xterm', x) %>%
        dplyr::rename_('id' = id)

    if (na.rm) data$data <- na.omit(data$data)

    # Set specifications for use later in the mason chain.
    data$y <- 'Yterm'
    data$x <- 'Xterm'
    data$covars <- covars
    if (!is.null(intvar)) data$intvar <- paste0('Xterm:', intvar)

    return(data)
}

#' @param y The outcome/dependent variable(s) for regression type designs, or a
#'   list of variables to compare with \code{x} for correlation designs.
#' @param x The predictor/exposure/independent variable(s) for regression type
#'   designs, or a list of variables to compare with \code{y} for correlation
#'   designs (or just a dataframe).
#' @param group In development.
#' @rdname lay_base
#' @export
#' @examples
#'
#' ds <- data.frame(state.region, state.x77)
#' ## Correlation
#' design(ds, 'cor') %>%
#'     lay_base(c('Income', 'Frost'), c('Population', 'Murder'))
lay_base.cor_df <- function(data,
                            x = names(data$data),
                            y = NULL,
                            group = NULL) {
    data$data <-
        data$data %>%
        dplyr::select_(.dots = c(x, y, group))

    # In development
    if (!is.null(group)) {
        stop('The group argument has not been implemented yet.')
#         data$data <-
#             data$data %>%
#             tidyr::gather_('')
    }

    # Set specification for use later in the mason chain.
    data$x <- paste(x)
    data$y <- y

    return(data)
}