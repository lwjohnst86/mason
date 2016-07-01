.is_not_empty <- function(x) {
    var.name <- deparse(substitute(x))
    if (length(x) == 0) {
        stop(
            paste0(
                'There is no ',
                var.name,
                ' variable(s) included,',
                ' please add it to the blueprint.'
            )
        )
    }
}

.is_blueprint <- function(x) {
    if(!'blueprint' %in% class(x)) {
        stop('Please supply a designed blueprint object (started using design).')
    }
}
.is_logic <- function(x)
    assertive::assert_is_logical(x)

.is_integer <- function(x)
    assertive::assert_is_integer(x)

.is_length <- function(x, n)
    assertive::assert_is_of_length(x, n)

.is_list <- function(list.object)
    assertive::assert_is_list(list.object)

.is_vector <- function(value)
    assertive::assert_is_vector(value)

.is_tbl_df <- function(data)
    assertive::assert_is_tbl_df(data)

.is_df <- function(data)
    assertive::assert_is_data.frame(data)

.is_string <- function(value)
    assertive::assert_is_a_string(value)

.is_numeric <- function(value)
    assertive::assert_is_numeric(value)

.is_character <- function(value)
    assertive::assert_is_character(value)

.is_function <- function(x)
    assertive::assert_is_function(x)
