
#' Add variables to the analysis
#'
#' While different analyses use different types of variables, in general they
#' can be classified as in the 'y' or 'x' position of a statistical equation.
#' They can further be classified as covariates and as an interaction term.
#'
#' @param blueprint The blueprint object
#' @param type The variable type, i.e. where it is located on the equation (y
#'   position, x, as a covariate, etc.)
#' @param variables Variables to use for the type specified
#'
#' @return Adds variables to the blueprint
#' @export
#'
#' @examples
#'
#' ds <- design(iris, 'cor')
#' ds <- add_variables(ds, 'xvar', 'Sepal.Length')
#' ds <- add_variables(ds, 'yvar', 'Petal.Length')
#'
#' ds <- design(iris, 't.test')
#' ds <- add_variables(ds, 'yvar', c('Sepal.Length', 'Sepal.Width'))
#' ds <- add_variables(ds, 'xvar', 'Petal.Length')
#'
add_variables <- function(blueprint,
                          type = c('yvars', 'xvars', 'covariates', 'interaction'),
                          variables) {
    .is_blueprint(blueprint)
    type <- match.arg(type)

    if (exists(type, blueprint))
        message(paste0(type, ' has already been added to blueprint... replacing it'))

    blueprint[[type]] <- variables
    return(blueprint)
}

