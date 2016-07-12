
#' Add variables to the analysis
#'
#' While different analyses use different types of variables, in general they
#' can be classified as in the 'y' or 'x' position of a statistical equation.
#' They can further be classified as covariates and as an interaction term.
#'
#' @param data The blueprint data object.
#' @param type The variable type, i.e. where it is located on the equation (y
#'   position, x, as a covariate, etc.)
#' @param variables Variables to use for the type specified
#'
#' @return Adds variables to the blueprint
#' @export
#'
#' @examples
#'
#' library(magrittr)
#' ds <- design(iris, 'cor') %>%
#'  add_settings()
#' add_variables(ds, 'xvar', 'Sepal.Length')
#' add_variables(ds, 'yvar', 'Petal.Length')
#'
#' ds <- design(iris, 't.test')
#' ds <- add_variables(ds, 'yvar', c('Sepal.Length', 'Sepal.Width'))
#' ds <- add_variables(ds, 'xvar', 'Petal.Length')
#'
add_variables <- function(data,
                          type = c('yvars', 'xvars', 'covariates', 'interaction'),
                          variables) {
    UseMethod('add_variables', data)
}

#' @export
add_variables.default <-
    function(data,
             type = c('yvars', 'xvars', 'covariates', 'interaction'),
             variables) {

        type <- match.arg(type)
        if (missing(variables))
            stop('Please supply one or more variables.')
        vars_exist(data, variables)

        if (any(c('xvars', 'yvars') %in% type)) {
            var.type <- sapply(data[variables], class)
            var.type <- gsub('integer', 'numeric', var.type)
            var.type <- unique(var.type)
            if (length(var.type) > 1)
                stop('Please do not mix numeric and character/factor variables',
                     ' in the ', type, '.', call. = FALSE)
        }

        var.specs <- attr(data, 'specs')$vars
        if (!is.null(var.specs))
            if (type %in% names(var.specs))
                message(type, ' already exists in the specs, but will be replaced.')

        make_blueprint(data, vars = stats::setNames(list(variables), type))
    }

