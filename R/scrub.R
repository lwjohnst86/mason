
#' @export
scrub <- function(blueprint, ...) {
    UseMethod('scrub', blueprint)
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

#' @export
polish_filter <- function(data, keep.pattern, column) {
    dplyr::filter(data, grepl(keep.pattern, data[[column]], ignore.case = TRUE))
}

#' @export
polish_transform_estimates <- function(data, transform.fun) {
    .is_function(transform.fun)
    dplyr::mutate_each(
        data,
        dplyr::funs(transform.fun),
        matches('estimate|std\\.error|conf\\.low|conf\\.high')
    )
}

#' @export
polish_adjust_pvalue <- function(data, method = 'BH') {
    dplyr::mutate(data, adj.p.value = p.adjust(p.value, method))
}