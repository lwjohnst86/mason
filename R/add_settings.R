#' Add analysis settings to the blueprint
#'
#' Most statistical techniques need to specify some settings for them to run.
#' This function sets those settings in the blueprint, before the statistical
#' method is used at the construction phase.
#'
#' @param data The blueprint data object.
#' @param ... Additional args.
#'
#' @return Settings for the analysis are added to the blueprint
#' @export
#'
#' @examples
#'
#' \dontrun{
#' design(iris, 'gee') %>%
#'  add_settings('Species', family = binomial('logit'), conf.int = FALSE)
#'
#' ds <- design(iris, 'cor')
#' ds <- add_settings(ds, method = 'spearman')
#'
#' ds <- design(iris, 't.test')
#' add_settings(ds, paired = TRUE)
#' add_settings(ds)
#' }
#'
add_settings <-
    function(data, ...) {
        UseMethod('add_settings', data)
    }

#' @rdname add_settings
#' @param cluster.id Variable that represents the cluster for GEE.
#' @param corstr The correlation structure. See [geepack::geeglm()].
#' @inheritParams stats::glm
#' @inheritParams broom::tidy.geeglm
#' @export
add_settings.gee_bp <-
    function(data,
             cluster.id,
             family,
             corstr = c('independence', 'exchangeable', 'ar1'),
             conf.int = TRUE,
             conf.level = 0.95, ...) {

        if (missing(family)) {
            family <- stats::gaussian()
        } else {
            if (class(family) != 'family')
                stop('Please use a family function (e.g. gaussian()).')
        }

        if (missing(cluster.id)) {
            stop('Please supply an ID for the cluster.', call. = FALSE)
        } else {
            vars_exist(data, cluster.id)
        }

        make_blueprint(data,
            family = family,
            corstr = match.arg(corstr),
            id = cluster.id,
            conf.int = conf.int,
            conf.level = conf.level
        )
    }

#' @rdname add_settings
#' @param hclust.order Whether to order the correlation data based on the
#'   [stats::hclust()] algorithm.
#' @inheritParams stats::cor
#' @export
add_settings.cor_bp <-
    function(data,
             method = c('pearson', 'kendall', 'spearman'),
             use = c('complete.obs',
                     'all.obs',
                     'pairwise.complete.obs',
                     'everything',
                     'na.or.complete'),
             hclust.order = FALSE,
             ...) {

        if (hclust.order)
            message('Note: hclust.order arg is still experimental')

        make_blueprint(data,
            method = match.arg(method),
            obs.usage = match.arg(use),
            hclust.order = hclust.order
        )
    }

#' @rdname add_settings
#' @export
add_settings.glm_bp <-
    function(data, family, conf.int = TRUE, conf.level = 0.95, ...) {

        if (missing(family)) {
            family <- stats::gaussian()
        } else {
            if (class(family) != 'family')
                stop('Please use a family function (e.g. gaussian()).')
        }
        make_blueprint(
            data,
            family = family,
            conf.int = conf.int,
            conf.level = conf.level
        )
    }

#' @rdname add_settings
#' @inheritParams pls::plsr
#' @param cv.data Whether to cross-validate the dataset into training and
#'   testing sets.
#' @param cv.seed Seed to set for cv.data.
#' @export
add_settings.pls_bp <-
    function(data,
             ncomp = NULL,
             scale = TRUE,
             validation = c('none', 'CV', 'LOO'),
             cv.data = TRUE,
             cv.seed = 1234,
             ...) {

        set.seed(cv.seed)
        n_rows <- nrow(data)
        if (cv.data) {
            cv_index <- sample(1:n_rows, size = floor(0.50 * n_rows))
        } else {
            cv_index <- NULL
        }

        make_blueprint(
            data,
            ncomp = ncomp,
            scale = scale,
            val = validation,
            cv.index = cv_index
            )
    }

#' @rdname add_settings
#' @inheritParams stats::t.test
#' @export
add_settings.t.test_bp <-
    function(data, paired = FALSE, ...) {
        make_blueprint(data,
            paired = paired
        )
    }
