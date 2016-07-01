library(dplyr)
testdata <- data.frame(state.region, state.x77) %>%
    dplyr::mutate(Rich = ifelse(Income > 4600, 'yes', 'no')) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(state.region)

devtools::use_data(testdata, overwrite = TRUE, internal = TRUE)