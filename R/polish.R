#' Polish the built structure
#'
#' Put the finishing touches to the finished structure. Used at the end of a
#' 'mason' project (ie. at the end of a mason pipe chain). Adjust p-values for
#' multiple comparisons, sift the results you want, and transform the beta
#' coefficients if needed.
#' @param data The data object output from the mason chain (after
#'   \code{\link{build}}).
#' @param sift.pattern The pattern to keep from the \code{terms} column of the
#'   mason dataframe.
#' @param adjust.p Adjust the p-value using the builtin \code{\link{p.adjust}}.
#' @param transform.beta.funs A function to transform the \code{estimate},
#'   \code{conf.low}, and \code{conf.high}.
#' @param rename.vars.funs Rename character variables and/or values using a
#'   function. Best combined with \code{\link{gsub}} to find and replace
#'   patterns or text.
#' @param ... Additional arguments
#'
#' @return Outputs a single dataframe
#' @export
#'
#' @examples
#'
#' ds <- data.frame(state.region, state.x77)
#' ## GEE
#' design(ds, 'gee', family = gaussian, corstr = 'exchangeable') %>%
#'     lay_base(id = 'state.region', c('Income', 'Frost'), c('Population', 'Murder'), 'Life.Exp') %>%
#'     build() %>%
#'     polish('Xterm$', TRUE, transform.beta.funs = function(x) exp(x),
#'          rename.vars.funs = function(x) gsub('Frost', 'f', x))
#' ## Correlation
#' design(ds, 'cor') %>%
#'     lay_base(c('Income', 'Frost'), c('Population', 'Murder')) %>%
#'     build() %>%
#'     polish()
polish <- function(data, ...) {
    UseMethod('polish', data)
}

#' @export
#'
polish.gee_df <- function(data,
                          sift.pattern = '*',
                          adjust.p = FALSE,
                          transform.beta.funs = function(x) x,
                          rename.vars.funs = function(x) x) {
    ds <- data$results %>%
        dplyr::filter(grepl(sift.pattern, term)) %>%
        dplyr::mutate_each(funs(transform.beta.funs),
                           matches('estimate|conf\\.low|conf\\.high')) %>%
        dplyr::mutate_each(funs(rename.vars.funs), Yterms, Xterms)

    if (adjust.p) ds <- dplyr::mutate(ds, p.value = p.adjust(p.value, 'BH'))

    class(ds) <- c(class(data), class(ds))
    return(ds)
}

#' @export
#'
polish.cor_df <- function(data,
                          rename.vars.funs = function(x) x) {
    ds <- data$results %>%
        tidyr::gather(Variables, Value, -.rownames) %>%
        dplyr::mutate_each(funs(rename.vars.funs), Variables, .rownames) %>%
        tidyr::spread(Variables, Value)

    class(ds) <- c(class(data), class(ds))
    return(ds)
}
