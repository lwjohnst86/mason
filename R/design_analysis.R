
#' Design the blueprint for an analysis.
#'
#' Sets up the initial design (i.e. the blueprint) of a statistical analysis to
#' use on the data. As in creating a building or structure, a blueprint is first
#' needed to guide the construction. This function \emph{only} creates that
#' blueprint, but does not do any construction (e.g. actually running statistics).
#'
#' @param data The dataset you want to analyze
#' @param test.type The type of statistical test to use
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
#'
design_analysis <- function(data, test.type = c('gee', 'cor', 'glm', 'pls', 'plsda')) {
    .is_df(data)
    .make_blueprint(
        data = dplyr::tbl_df(data),
        bp.class = paste0(match.arg(test.type), '_blueprint')
    )
}

.make_blueprint <- function(..., blueprint = NULL, bp.class = NULL) {
    plans <- list(...)
    if (!is.null(blueprint)) {
        blueprint <- modifyList(blueprint, plans)
    } else {
        bp.class <- c(bp.class, 'blueprint')
        blueprint <- structure(c(blueprint, plans),
                               class = bp.class)
    }
    return(blueprint)
}