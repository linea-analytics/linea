# libs   ####
library(testthat)
library(linea)
library(rlist)
library(dplyr)

# test_check("linea")

# set up ####
### NOT POOLED  ----

# import data
data = read_xcsv(verbose = F,
                 file = "https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv") %>%
  check_ts(verbose = F,
          allow_non_num = T,
          date_col = "date") %>%
  get_seasonality(verbose = F,
                  date_col_name = "date",
                  date_type = "weekly starting")


# vars
dv = "ecommerce"
ivs = c("black.friday", "christmas", "covid")
id_var = "date"

# model table
model_table = tibble(
  variable = ivs,
  decay = c(0.5, 0, 0),
  diminish = c(0, 0, 0),
  lag = c(0, 0, 0),
  ma = c(0, 0, 0)
)

# category
category = tibble(
  variable = c("christmas" , "christmas"),
  category = c("a", "a"),
  calc = c("", "")
)

# run model
model = run_model(
  verbose = F,
  data = data,
  dv = dv,
  model_table = model_table,
  normalise_by_pool = F
)



### POOLED      ----

# import data
pooled_data = read_xcsv(verbose = F,
                        file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv") %>%
  check_ts(verbose = F,
          allow_non_num = T,
          date_col = "Week") %>%
  get_seasonality(verbose = F,
                  date_col_name = "Week",
                  date_type = "weekly starting")

# meta data
pooled_meta_data = tibble(
  variable = c("amazon", "rakhi", "country", "Week"),
  meta = c("STA", "STA", "POOL", "ID")
)

# vars
pooled_dv = "amazon"
pooled_ivs = c("rakhi", "christmas", "diwali")
pooled_id_var = "Week"

# model table
pooled_model_table = build_model_table(c(pooled_ivs, "", ""))

# category
pooled_category = tibble(
  variable = c("rakhi" , "christmas"),
  category = c("a", "a"),
  calc = c("", "")
)

# run model
pooled_model = run_model(
  verbose = F,
  data = pooled_data,
  dv = pooled_dv,
  meta_data =  pooled_meta_data,
  model_table = pooled_model_table,
  normalise_by_pool = T
)



# tests  ####
### read data   ####

test_that('read data',{


  data = read_xcsv(verbose = F,
                   file = "https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")%>%
    is.data.frame() %>%
    expect_equal(T)

})
test_that('read data - pooled',{

  pooled_data = read_xcsv(verbose = F,
                          file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv")%>%
    is.data.frame() %>%
    expect_equal(T)

})
test_that('read data - pooled ts',{

  pooled_data = read_xcsv(verbose = F,
                          file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv")%>%
    check_ts(date_col = 'Week') %>%
    is.data.frame() %>%
    expect_equal(T)

})


### seasonality ####

test_that('seasonality',{

  pooled_data = read_xcsv(verbose = F,
                          file = "https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")%>%
    check_ts(date_col = 'date') %>%
    get_seasonality(date_col_name = 'date',
                    date_type = 'weekly starting') %>%
    is.data.frame() %>%
    expect_equal(T)

})
test_that('seasonality - pooled',{

  pooled_data = read_xcsv(verbose = F,
                          file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv")%>%
    check_ts(date_col = 'Week') %>%
    get_seasonality(date_col_name = 'Week',
                    pool_var = 'country',
                    date_type = 'weekly starting') %>%
    is.data.frame() %>%
    expect_equal(T)

})


### run model   ####

test_that('run_model ivs, dv',{

  run_model(data = data, dv = dv, ivs = ivs) %>%
    class() %>%
    expect_equal('lm')

})

### what next   ---------------------------------------------------------------

test_that("what next - output dataframe", {
  model %>% 
    what_next() %>%
    is.data.frame() %>%
    expect_equal(T)
})
test_that("what next - output not all na", {
  model %>% 
    what_next() %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(F)
})

test_that("what next - output dataframe - diff F - not pooled", {
  model %>% 
    what_next(r2_diff = F) %>%
    is.data.frame() %>%
    expect_equal(T)
})
test_that("what next - output not all na - diff F - not pooled", {
  model %>% 
    what_next(r2_diff = F) %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(F)
})

test_that("what next - pooled - output dataframe", {
  pooled_model %>% 
    what_next() %>%
    is.data.frame() %>%
    expect_equal(T)
})
test_that("what next - pooled - output not all na", {
  pooled_model %>% 
    what_next() %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(F)
})

### get gt      ------------------------------------------------------------------

test_that("gtrends_f - pooled - output dataframe",{
  gt_f(data = pooled_data,
       kw = 'bitcoin',
       date_col = pooled_id_var) %>%
    is.data.frame() %>%
    expect_equal(T)
})


### resp curves ------------------------------------------------------------------

test_that("response curves - pooled - output is plotly",{

  pooled_model %>%
    response_curves() %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(T)
})


test_that("response curves - not pooled - output is dataframe",{

  model %>%
    response_curves(table = T) %>%
    is.data.frame() %>%
    expect_equal(T)

})
