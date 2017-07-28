## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(mason)

## ----ttests--------------------------------------------------------------
swiss %>% 
    design('t.test') %>% 
    add_settings() %>% 
    add_variables('yvars', c('Fertility', 'Agriculture')) %>% 
    add_variables('xvars', c('Examination', 'Education')) %>% 
    construct() %>% 
    scrub()

## ----cor-----------------------------------------------------------------
swiss %>% 
    design('cor') %>% 
    add_settings() %>% 
    add_variables('yvars', c('Fertility', 'Agriculture')) %>% 
    add_variables('xvars', c('Examination', 'Education')) %>% 
    construct() %>% 
    scrub()

## ----glm-----------------------------------------------------------------
swiss %>% 
    design('glm') %>% 
    add_settings() %>% 
    add_variables('yvars', c('Fertility', 'Agriculture')) %>% 
    add_variables('xvars', c('Examination', 'Education')) %>% 
    add_variables('covariates', 'Catholic') %>% 
    construct() %>% 
    scrub()

## ----gee-----------------------------------------------------------------
data.frame(state.x77, state.region) %>% 
    design('gee') %>% 
    add_settings(cluster.id = 'state.region') %>% 
    add_variables('yvars', c('Income', 'Frost')) %>% 
    add_variables('xvars', c('Population', 'Murder')) %>% 
    add_variables('covariates', c('Life.Exp', 'Area')) %>% 
    add_variables('interaction', 'Area') %>% 
    construct() %>% 
    add_variables('xvars', c('Illiteracy')) %>%
    construct() %>%
    scrub()

## ----plsr----------------------------------------------------------------
swiss %>% 
    design('pls') %>% 
    add_settings(validation = 'CV', cv.data = TRUE) %>% 
    add_variables('yvars', c('Agriculture')) %>% 
    add_variables('xvars', c('Examination', 'Education', 'Fertility')) %>% 
    construct() %>% 
    scrub() %>% 
    summary()

