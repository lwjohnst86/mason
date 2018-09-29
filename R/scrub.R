
#' Scrub down and tidy up the constructed analysis results.
#'
#' @param data The blueprint data object.
#' @param output Selecting what to output from model.
#' @param ... Other arguments passed to methods.
#'
#' @return Outputs a cleaned up version of the constructed analysis.
#' @export
#' @seealso See also [`tidy_up()`] for pls tidying.
#'
#' @examples
#'
#' ds <- design(iris, 'cor')
#' ds <- add_settings(ds)
#' ds <- add_variables(ds, 'xvars', c('Sepal.Length', 'Sepal.Width'))
#' ds <- construct(ds)
#' scrub(ds)
#'
scrub <- function(data, ...) {
    UseMethod('scrub', data)
}

#' @export
scrub.default <- function(data, ...) {
    dplyr::tbl_df(attr(data, 'specs')$results)
}

#' @export
scrub.gee_bp <- function(data, ...) {
    results <- attr(data, 'specs')$results
    mutate_tool <- lazyeval::interp("gsub('XtermValues', '<-Xterm', term)")
    results %>%
        dplyr::mutate_(.dots = stats::setNames(mutate_tool, 'term')) %>%
        dplyr::tbl_df()
}

#' @export
scrub.glm_bp <- scrub.gee_bp

#' @export
scrub.cor_bp <- function(data, ...) {
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

#' @export
#' @rdname scrub
scrub.pls_bp <- function(data, output = c("mvr_object", "default", "scores", "loadings", "score_corr", "explained_var"), ...) {
    output <- match.arg(output)
    model <- attr(data, 'specs')$results
    if (output == "mvr_object") {
        .result <- model
    } else {
        .result <- tidy_up(model = model, output = output)
    }

    .result
}

