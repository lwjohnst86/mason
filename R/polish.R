#' Polish the built structure
#'
#' Put the finishing touches to the finished structure. Used at the end of a
#' 'mason' project (ie. at the end of a mason pipe chain). Adjust p-values for
#' multiple comparisons, filter the results you want, and transform the beta
#' coefficients if needed.
#' @param data The data object output from the mason chain (after
#'   \code{\link{build}}).
#' @param ... Additional arguments
#'
#' @return Outputs a single dataframe
#' @export
polish <- function(data, ...) {
    UseMethod('polish', data)
}

#' @param keep.pattern The pattern to keep from the \code{terms} column of the
#'   mason dataframe.
#' @param adjust.p Adjust the p-value using the builtin \code{\link{p.adjust}}.
#' @param transform.beta.funs A function to transform the \code{estimate},
#'   \code{conf.low}, and \code{conf.high}.
#' @rdname polish
#' @export
#' @examples
#'
#' ds <- data.frame(state.region, state.x77)
#' ## GEE
#' design(ds, 'gee', family = gaussian, corstr = 'exchangeable') %>%
#'     lay_base(id = 'state.region', c('Income', 'Frost'), c('Population', 'Murder'), 'Life.Exp') %>%
#'     build() %>%
#'     polish('Xterm$', TRUE, transform.beta.funs = function(x) exp(x),
#'          rename.vars.funs = function(x) gsub('Frost', 'f', x))
#'
polish.gee_df <- function(data,
                          keep.pattern = '*',
                          adjust.p = FALSE,
                          transform.beta.funs = function(x) x,
                          rename.vars.funs = function(x) x) {
    ds <- data$results %>%
        dplyr::filter(grepl(keep.pattern, term)) %>%
        dplyr::mutate_each(funs(transform.beta.funs),
                           matches('estimate|std\\.error|conf\\.low|conf\\.high')) %>%
        dplyr::mutate_each(funs(rename.vars.funs), Yterms, Xterms)

    if (adjust.p) ds <- dplyr::mutate(ds, p.value = p.adjust(p.value, 'BH'))

    class(ds) <- c(class(data), class(ds))
    return(ds)
}

#' @param rename.vars.funs Rename character variables and/or values using a
#'   function. Best combined with \code{\link{gsub}} to find and replace
#'   patterns or text.
#' @rdname polish
#' @export
#' @examples
#'
#' ds <- data.frame(state.region, state.x77)
#' ## Correlation
#' design(ds, 'cor') %>%
#'     lay_base(c('Income', 'Frost'), c('Population', 'Murder')) %>%
#'     build() %>%
#'     polish()
#'
polish.cor_df <- function(data,
                          rename.vars.funs = function(x) x) {
    ds <- data$results %>%
        dplyr::rename_('Xvar' = '.rownames') %>%
        reshape2::melt(id.vars = 'Xvar',
                       measure.vars = which(!names(.) %in% 'Xvar', arr.ind = TRUE),
                       variable.name = 'Yvar',
                       value.name = 'Value') %>%
        dplyr::mutate(Xvar = rename.vars.funs(Xvar),
                      Yvar = rename.vars.funs(Yvar)) %>%
        dplyr::filter(Xvar != Yvar) %>%
        na.omit()

    class(ds) <- c(class(data), class(ds))
    return(ds)
}
