# Scrub -------------------------------------------------------------------

#' Scrub down and polish up the constructed analysis results.
#'
#' @param data The blueprint data object.
#'
#' @return Outputs a cleaned up version of the constructed analysis.
#' @export
#'
#' @examples
#'
#' ds <- design(iris, 'cor')
#' ds <- add_settings(ds)
#' ds <- add_variables(ds, 'xvars', c('Sepal.Length', 'Sepal.Width'))
#' ds <- construct(ds)
#' scrub(ds)
#'
scrub <- function(data) {
    UseMethod('scrub', data)
}

#' @export
scrub.default <- function(data) {
    dplyr::tbl_df(attr(data, 'specs')$results)
}

#' @export
scrub.gee_bp <- function(data) {
    results <- attr(data, 'specs')$results
    mutate_tool <- lazyeval::interp("gsub('XtermValues', '<-Xterm', term)")
    results %>%
        dplyr::mutate_(.dots = stats::setNames(mutate_tool, 'term')) %>%
        dplyr::tbl_df()
}

#' @export
scrub.glm_bp <- scrub.gee_bp

#' @export
scrub.cor_bp <- function(data) {
    results <- attr(data, 'specs')$results %>%
        dplyr::rename_('Vars1' = 'Variables')
    vars <- names(results)
    vars <- setdiff(vars, 'Vars1')

    results %>%
        tidyr::gather_('Vars2', 'Correlations', vars) %>%
        dplyr::filter_(.dots = lazyeval::interp("Vars1 != Vars2")) %>%
        stats::na.omit() %>%
        dplyr::tbl_df()
}

# Polish ------------------------------------------------------------------

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

#' @describeIn polish \code{polish_renaming} simply takes a function, most
#'   likely one that uses \code{\link[base]{gsub}}, and uses that to search and
#'   replace words, etc, in the specified columns.
#' @param renaming.fun A function, typically with \code{\link[base]{gsub}}, that
#'   searches and replaces strings.
#' @param columns The columns to apply the renaming function to. Defaults to
#'   columns that are a factor or character vectors.
#' @export
polish_renaming <- function(data, renaming.fun, columns = NULL) {
    assertive::assert_is_function(renaming.fun)

    if (is.null(columns)) {
        columns <-
            {sapply(data, function(x) {
                is.character(x) | is.factor(x)
                }) &
            !names(data) %in% 'p.value'} %>%
            which() %>%
            names()
    } else {
        assertive::assert_is_character(columns)
    }

    dplyr::mutate_each_(data, dplyr::funs(renaming.fun), columns)
}

#' @describeIn polish \code{polish_filter} is basically a thin wrapper around
#' \code{\link[dplyr]{filter}}, but using \code{\link[base]{grepl}} for the
#' pattern searching.
#'
#' @param keep.pattern Rows to keep based on a regular expression pattern.
#' @param column The column to apply the filtering to.
#' @export
polish_filter <- function(data, keep.pattern, column) {
    dplyr::filter(data, grepl(keep.pattern, data[[column]], ignore.case = TRUE))
}

#' @describeIn polish \code{polish_transform_estimates} is simply a thin wrapper
#'   around \code{\link[dplyr]{mutate_each}}.
#'
#' @param transform.fun A function to modify continuous variable columns.
#' @export
polish_transform_estimates <- function(data, transform.fun) {
    assertive::assert_is_function(transform.fun)
    dplyr::mutate_each(
        data,
        dplyr::funs(transform.fun),
        dplyr::matches('estimate|std\\.error|conf\\.low|conf\\.high')
    )
}

#' @describeIn polish \code{polish_adjust_pvalue} is a thin wrapper around
#'   \code{\link[dplyr]{mutate}} and \code{\link[stats]{p.adjust}}
#'
#' @param method Correction method for the p-value adjustment
#'   (\code{\link[stats]{p.adjust}}).
#' @export
polish_adjust_pvalue <- function(data, method = 'BH') {
    mutate_tool <- lazyeval::interp('stats::p.adjust(p.value, method)',
                                    method = method)
    dplyr::mutate_(data, .dots = stats::setNames(mutate_tool, 'adj.p.value'))
}