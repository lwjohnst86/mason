#' Design the masonry project
#'
#' This is the blueprint, or design, stage of the analysis. This function sets
#' the data and specifications for use later in the masonry chain
#' (\code{\link{lay_base}}, \code{\link{build}}, and than \code{\link{polish}}).
#' @param data Raw data to process for analysis.
#' @param test.type The test or analysis to run.
#' @param method Which correlation method to use.
#' @param use How to handle missing values in correlation.
#' @param family Distribution family to use for \code{\link[geepack]{geeglm}}.
#' @param corstr Correlation structure to use for \code{\link[geepack]{geeglm}}.
#' @param ... Additional parameters.
#'
#' @return Outputs a list with the raw data and specifications for use later in
#'   the masonry chain.
#' @export
#'
#' @examples
#' ds <- data.frame(state.region, state.x77)
#' ## GEE
#' design(ds, 'gee', family = gaussian, corstr = 'exchangeable')
#' ## Correlation
#' design(ds, 'cor', method = 'spearman')
design <- function(data,
                   test.type,
                   ...) {
    request <- data.frame()
    class(request) <- match.arg(test.type, c('gee', 'cor'))
    .fetch_design(data, request, ...)
}

#' @export
#'
.fetch_design <- function(data,
                          request,
                          ...) {
    UseMethod('.fetch_design', request)
}

#' @export
#'
.fetch_design.gee <- function(data,
                              request,
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

#' @export
#'
.fetch_design.cor <- function(data,
                              request,
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

