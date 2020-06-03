
#' Do some final polishing of the scrubbed mason analysis data.
#'
#' @name polish
#' @param data The scrubbed object.
#' @examples
#' library(magrittr)
#' ds <- swiss %>%
#'  design('glm') %>%
#'  add_settings() %>%
#'  add_variables('yvar', c('Fertility', 'Education')) %>%
#'  add_variables('xvar', c('Agriculture', 'Catholic')) %>%
#'  add_variables('covariates', 'Examination') %>%
#'  construct() %>%
#'  scrub()
#' polish_renaming(ds, function(x) gsub('Education', 'Schooling', x))
#' polish_filter(ds, 'Xterm', 'term')
#' polish_adjust_pvalue(ds)[c('p.value', 'adj.p.value')]
#' polish_transform_estimates(ds, function(x) exp(x))
NULL

#' @describeIn polish `polish_renaming` simply takes a function, most
#'   likely one that uses [base::gsub()], and uses that to search and
#'   replace words, etc, in the specified columns.
#' @param renaming.fun A function, typically with [base::gsub()], that
#'   searches and replaces strings.
#' @param columns The columns to apply the renaming function to. Defaults to
#'   columns that are a factor or character vectors.
#' @export
polish_renaming <- function(data, renaming.fun, columns = NULL) {
    stopifnot(is.function(renaming.fun))

    if (is.null(columns)) {
        columns <-
            {sapply(data, function(x) {
                is.character(x) | is.factor(x)
                }) &
            !names(data) %in% 'p.value'} %>%
            which() %>%
            names()
    } else {
        stopifnot(is.character(columns))
    }

    dplyr::mutate(data, dplyr::across(columns, renaming.fun))
}

#' @describeIn polish `polish_filter` is basically a thin wrapper around
#' [dplyr::filter()], but using [base::grepl()] for the
#' pattern searching.
#'
#' @param keep.pattern Rows to keep based on a regular expression pattern.
#' @param column The column to apply the filtering to.
#' @export
polish_filter <- function(data, keep.pattern, column) {
    dplyr::filter(data, grepl(keep.pattern, data[[column]], ignore.case = TRUE))
}

#' @describeIn polish `polish_transform_estimates` is simply a thin wrapper
#'   around [dplyr::mutate()].
#'
#' @param transform.fun A function to modify continuous variable columns.
#' @export
polish_transform_estimates <- function(data, transform.fun) {
    stopifnot(is.function(transform.fun))
    dplyr::mutate(data,
                  dplyr::across(
                      dplyr::matches('estimate|std\\.error|conf\\.low|conf\\.high'),
                      transform.fun
                  ))
}

#' @describeIn polish `polish_adjust_pvalue` is a thin wrapper around
#'   [dplyr::mutate()] and [stats::p.adjust()]
#'
#' @param method Correction method for the p-value adjustment
#'   ([stats::p.adjust()]).
#' @export
polish_adjust_pvalue <- function(data, method = 'BH') {
    dplyr::mutate(data, adj.p.value = stats::p.adjust(.data$p.value, method = method))
}