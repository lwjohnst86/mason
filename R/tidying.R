
#' Convert model output to tidy tibble/dataframe.
#'
#' Currently this only tidies up PLS model objects. The main important output
#' objects from the PLS model are:
#'
#' - Scores: These are the individual scores calculated from the model for each
#' observation. Use these to look for patterns between components or between
#' X or Y variables.
#' - Loadings: These are the combined weights in the model (including both X and
#' Y). Strongly correlated X variables that underlie Y will have similar
#' loadings.
#' - Explained variance: This is the amount of variance that an individual
#' component explains within X. This is useful to use to see which components to
#' keep.
#'
#' @param model The model object.
#' @param output Which output to choose from model.
#' @param ... Not currently used. For later method additions.
#'
#' @return Tibble object with tidied model output. There are several output options:
#'
#' - default: Tibble with five columns for the x variables, components,
#' loadings, scores to variable correlations, and explained variance for each
#' component and x variable combination.
#' - loadings, score_corr: Tibble with three columns for x variables,
#' components, and either loadings or score to variable correlations.
#' - explained_var: Tibble with two columns for component and it's explained
#' variance.
#' - scores: Tibble with one column for each component, with values for the scores
#' for each observation.
#'
#' @export
#' @seealso See this [website](https://learnche.org/pid/latent-variable-modelling/projection-to-latent-structures/interpreting-pls-scores-and-loadings)
#' for more details on how to interpret the results of PLS.
#'
#' @examples
#'
#' library(pls)
#' data(yarn)
#'
#' NIR <- yarn$NIR
#' density <- yarn$density
#' model <- plsr(density ~ NIR)
#' tidy_up(model)
#' tidy_up(model, "loadings")
#' tidy_up(model, "scores")
#' tidy_up(model, "score_cor")
#' tidy_up(model, "explained_var")
#'
tidy_up <- function(model, output, ...) {
    UseMethod("tidy_up", model)
}

#' @export
tidy_up.default <- function(model, ...) {
    stop("tidy_up is currently only set for pls analysis. Don't use this for anything else for now.")
}

# TODO: Include a predict tidying? Or residuals?
# TODO: Include RMSEP in tidied up form too?
#' @export
tidy_up.mvr <- function(model, output = c("default", "scores", "loadings", "score_corr", "explained_var"), ...) {
    if (!requireNamespace("pls", quietly = TRUE)) {
        stop("Please install the pls package to run this analysis.")
    }

    output <- match.arg(output)
    switch(
        output,
        default = {
            loadings_and_corr <- dplyr::bind_cols(
                pls_loadings_as_dataframe(model),
                pls_score_correlations_as_dataframe(model)["scores.to.var.corr"]
            )
            loadings_and_corr_with_explvar <- loadings_and_corr %>%
                dplyr::full_join(pls_explained_variance_as_dataframe(model),
                                 by = "components")
            loadings_and_corr_with_explvar
        },
        loadings = pls_loadings_as_dataframe(model),
        scores = pls_scores_as_dataframe(model),
        score_corr = pls_score_correlations_as_dataframe(model),
        explained_var = pls_explained_variance_as_dataframe(model)
    )
}

pls_loadings_as_dataframe <- function(model) {
    .as_dataframe <- model %>%
        pls::loadings() %>%
        unclass() %>%
        tibble::as_tibble(rownames = "xvariables")

    .as_long_dataframe <- .as_dataframe %>%
        tidyr::gather("components", "loadings", -.data$xvariables) %>%
        dplyr::mutate(components = rename_components(.data$components))

    .as_long_dataframe
}

pls_scores_as_dataframe <- function(model) {
    .scores <- pls::scores(model)
    attr(.scores, "explvar") <- NULL

    .as_dataframe <- .scores %>%
        unclass() %>%
        as.matrix() %>%
        tibble::as_tibble() %>%
        dplyr::rename_all(rename_component_columns)

    .as_dataframe
}

pls_scores_to_original_data <- function(model) {
    .original_data <- dplyr::bind_cols(
        tibble::as_tibble(model$model$X),
        tibble::as_tibble(model$model$Y)
    )

    .scores <- pls_scores_as_dataframe(model)

    dplyr::bind_cols(.original_data, .scores)
}

pls_explained_variance_as_dataframe <- function(model) {
    explained_variance <- pls::explvar(model)
    .as_df <- tibble::enframe(explained_variance,
                              name = "components",
                              value = "explained.variance")

    .as_df %>%
        dplyr::mutate(components = rename_components(.data$components))
}

pls_score_correlations_as_dataframe <- function(model) {
    .xvariables <- stats::model.matrix(model)
    .scores <- pls_scores_as_dataframe(model)
    correlations <- stats::cor(.xvariables, .scores)
    .as_long_dataframe <- tibble::as_tibble(correlations,
                                            rownames = "xvariables") %>%
        tidyr::gather("components", "scores.to.var.corr", -.data$xvariables)

    .as_long_dataframe %>%
        dplyr::mutate(components = trimws(gsub("scores|\\.", " ", .data$components)))
}

rename_component_columns <- function(columns) {
    sub("^Comp ", "scores.component.", columns)
}

rename_components <- function(columns) {
    sub("^Comp ", "component ", columns)
}
