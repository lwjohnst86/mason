
# Scrub -------------------------------------------------------------------

#' Scrub down and polish up the constructed analysis results.
#'
#' @param blueprint The blueprint object.
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
scrub <- function(blueprint) {
    UseMethod('scrub', blueprint)
}

#' @export
scrub.default <- function(blueprint) {
    results <- blueprint$results %>%
        dplyr::tbl_df()
    return(results)
}

#' @export
scrub.gee_blueprint <- function(blueprint) {
    blueprint$results %>%
        dplyr::mutate(term = gsub('XtermValues', '<-Xterm', term)) %>%
        dplyr::tbl_df()
}

#' @export
scrub.glm_blueprint <- scrub.gee_blueprint

#' @export
scrub.cor_blueprint <- function(blueprint) {
    results <- blueprint$results %>%
        dplyr::rename_('Vars1' = '.rownames') %>%
        tidyr::gather('Vars2', 'Correlations', -Vars1) %>%
        dplyr::filter(Vars1 != Vars2) %>%
        na.omit() %>%
        dplyr::tbl_df()

    return(results)
}

# Polish ------------------------------------------------------------------

#' Do some final polishing of the scrubbed mason analysis data.
#'
#' @name polish
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

#' @param data The scrubbed object.
#' @param renaming.fun A function, typically with \code{\link[base]{gsub}}, that
#'   searches and replaces strings.
#' @param columns The columns to apply the renaming function to. Defaults to
#'   columns that are a factor or character vectors.
#' @rdname polish
#' @export
polish_renaming <- function(data, renaming.fun, columns = NULL) {
    .is_function(renaming.fun)

    if (is.null(columns)) {
        columns <-
            {sapply(data, function(x) {
                is.character(x) | is.factor(x)
                }) &
            !names(data) %in% 'p.value'} %>%
            which() %>%
            names()
    } else {
        .is_character(columns)
    }

    dplyr::mutate_each_(data, dplyr::funs(renaming.fun), columns)
}

#' \code{polish_filter} is basically a thin wrapper around
#' \code{\link[dplyr]{filter}}, but using \code{\link[base]{grepl}} for the
#' pattern searching.
#'
#' @param data The scrubbed results.
#' @param keep.pattern Rows to keep based on a regular expression pattern.
#' @param column The column to apply the filtering to.
#' @rdname polish
#' @export
polish_filter <- function(data, keep.pattern, column) {
    dplyr::filter(data, grepl(keep.pattern, data[[column]], ignore.case = TRUE))
}

#' \code{polish_transform_estimates} is simply a thin wrapper around
#' \code{\link[dplyr]{mutate_each}}.
#'
#' @param data The scrubbed results.
#' @param transform.fun A function to modify continuous variable columns.
#' @rdname polish
#' @export
polish_transform_estimates <- function(data, transform.fun) {
    .is_function(transform.fun)
    dplyr::mutate_each(
        data,
        dplyr::funs(transform.fun),
        matches('estimate|std\\.error|conf\\.low|conf\\.high')
    )
}

#' \code{polish_adjust_pvalue} is a thin wrapper around
#' \code{\link[dplyr]{mutate}} and \code{\link[stats]{p.adjust}}
#'
#' @param data The scrubbed results.
#' @inheritParams stats::p.adjust
#' @rdname polish
#' @export
polish_adjust_pvalue <- function(data, method = 'BH') {
    dplyr::mutate(data, adj.p.value = p.adjust(p.value, method))
}