.regression_data_prep <- function(data, y, x, covars = NULL,
                                  int = NULL, id = NULL) {
    data <- data %>%
        dplyr::select_(.dots = c(id, y, x, covars, int)) %>%
        tidyr::gather_('Yterms', 'YtermValues', y) %>%
        tidyr::gather_('Xterms', 'XtermValues', x)

    if (!is.null(id))
        data <- dplyr::rename_(data, 'id' = id)

    data <- data %>%
        dplyr::group_by(Yterms, Xterms)

    return(data)
}

.append_if_results_exist <- function(blueprint, results) {
    .is_blueprint(blueprint)
    .is_tbl_df(results)

    if (exists("results", where = blueprint)) {
        blueprint$results <- dplyr::bind_rows(blueprint$results, results)
    } else {
        blueprint$results <- results
    }

    return(blueprint)
}

print.blueprint <- function(x, ...) {
    if (is.null(x$results)) {
        cat("Analysis under construction, showing data right now:\n",
            "- statistic method:", x$stat, '\n\n')

        print(x$data)
    } else if (!is.null(x$results)) {
        cat('Analysis for', x$stat, 'constructed, here are the results:\n')
        print(x$results)
    } else {
        cat('Nothing to show yet, is something wrong maybe?')
    }
}

#' @importFrom magrittr "%>%"
#' @export
magrittr::`%>%`
