
#' Design the blueprint for an analysis.
#'
#' Sets up the initial design (i.e. the blueprint) of a statistical analysis to
#' use on the data. As in creating a building or structure, a blueprint is first
#' needed to guide the construction. This function *only* creates that
#' blueprint, but does not do any construction (e.g. actually running statistics).
#'
#' @param data The dataset you want to analyze
#' @param statistic The type of statistical test to use
#'
#' @return Creates a blueprint object that will be used to construct the
#'   analysis in a later phase.
#' @export
#'
#' @examples
#'
#' design(iris, 'gee')
#' design(iris, 'cor')
#' design(iris, 'glm')
#' design(iris, 't.test')
#'
design <- function(data,
                   statistic = c('gee', 'cor', 'glm', 'pls', 't.test')) {
    stopifnot(is.data.frame(data))
    type <- match.arg(statistic)
    make_blueprint(
        data = data,
        stat = type,
        type = paste0(type, '_bp')
    )
}

make_blueprint <- function(data, ..., type = NULL) {
    specs <- list(...)
    if (!is.null(attr(data, 'specs'))) {
        specs <- utils::modifyList(attr(data, 'specs'), specs)
    }

    if (!'bp' %in% class(data)) {
        class(data) <- c('bp', type, class(data))
    }

    attr(data, 'specs') <- specs
    return(data)
}
