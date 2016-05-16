
#' @export
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

