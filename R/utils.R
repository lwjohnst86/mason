# experimental/not complete.
as_blueprint <- function(x) {
    class(x) <- c(class(x), 'blueprint')
}

.tidy_cor_results <- function(cor.data, hclust.order = TRUE, blueprint = blueprint) {
    .is_blueprint(blueprint)

    if (hclust.order & is.null(blueprint$yvar)) {
        # Taken from
        # http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
        message('still experimental')
        ordering <- hclust(as.dist((1 - cor.data) / 2))
        cor.data <- cor.data[ordering$order, ordering$order]
    }

    if (is.null(blueprint$yvar))
        cor.data[upper.tri(cor.data)] <- NA

    broom::tidy(cor.data)
}

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
        cat("Analysis under construction, showing data right now:\n")
        print(head(x$data, 5))
    } else if (!is.null(x$results)) {
        cat('Analysis constructed, here are the results:\n')
        print(head(x$results, 5))
    } else {
        cat('Nothing to show yet, is something wrong maybe?')
    }
}