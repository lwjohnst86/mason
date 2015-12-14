#' Design the masonry project
#'
#' This is the blueprint, or design, stage of the analysis. This function sets
#' the data and specifications for use later in the masonry chain
#' (\code{\link{lay_base}}, \code{\link{build}}, and than \code{\link{polish}}).
#' @param data Raw data to process for analysis.
#' @param test.type The test or analysis to run.
#' @param ... Additional parameters.
#'
#' @return Outputs a list with the raw data and specifications for use later in
#'   the masonry chain.
#' @export
design <- function(data,
                   test.type,
                   ...) {
    class(test.type) <- match.arg(test.type, c('gee', 'cor', 'lm'))
    .fetch_design(data, test.type, ...)
}

#' @export
.fetch_design <- function(data,
                          test.type,
                          ...) {
    UseMethod('.fetch_design', test.type)
}

#' @param family Distribution family to use for \code{\link[geepack]{geeglm}}.
#' @param corstr Correlation structure to use for \code{\link[geepack]{geeglm}}.
#' @rdname design
#' @export
#' @examples
#'
#' ds <- data.frame(state.region, state.x77)
#' ## GEE
#' design(ds, 'gee', family = gaussian, corstr = 'exchangeable')
.fetch_design.gee <- function(data,
                              test.type,
                              family = gaussian('identity'),
                              corstr = c('independence', 'exchangeable', 'ar1')) {
    data <-
        structure(list(
            data = dplyr::tbl_df(data),
            family = family,
            corstr = match.arg(corstr)
        ),
        class = 'gee_df')
    return(data)
}

#' @param method Which correlation method to use.
#' @param use How to handle missing values in correlation.
#' @rdname design
#' @export
#' @examples
#'
#' ds <- data.frame(state.region, state.x77)
#' ## Correlation
#' design(ds, 'cor', method = 'spearman')
.fetch_design.cor <- function(data,
                              test.type,
                              method = c('pearson', 'kendall', 'spearman'),
                              use = c('complete.obs', 'all.obs',
                                      'pairwise.complete.obs', 'everything',
                                      'na.or.complete')) {
    data <-
        structure(list(
            data = dplyr::tbl_df(data) %>%
                dplyr::select(which(sapply(data, is.numeric))),
            method = match.arg(method),
            use = match.arg(use)
        ),
        class = 'cor_df')

    return(data)
}

#' @rdname design
#' @export
#' @examples
#'
#'## lm
#'ds <- data.frame(state.region, state.x77)
#'design(ds, 'lm')
#'
.fetch_design.lm <- function(data, test.type) {
    data <-
        structure(list(
            data = dplyr::tbl_df(data)
        ),
        class = 'lm_df')
    return(data)
}