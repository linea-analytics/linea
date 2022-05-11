# libs   ####
library(testthat)
library(linea)
library(rlist)
library(dplyr)

# test_check("linea")

# set up ####
### NOT POOLED  ----

# import data
data = read_xcsv(verbose = FALSE,
                 file = "https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv") %>%
  check_ts(verbose = FALSE,
          allow_non_num = TRUE,
          date_col = "date") %>%
  get_seasonality(verbose = FALSE,
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
  verbose = FALSE,
  data = data,
  dv = dv,
  model_table = model_table,
  normalise_by_pool = FALSE
)



### POOLED      ----

# import data
pooled_data = read_xcsv(verbose = FALSE,
                        file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv") %>%
  check_ts(verbose = FALSE,
          allow_non_num = TRUE,
          date_col = "Week") %>%
  get_seasonality(verbose = FALSE,
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
  verbose = FALSE,
  data = pooled_data,
  dv = pooled_dv,
  meta_data =  pooled_meta_data,
  model_table = pooled_model_table,
  normalise_by_pool = TRUE
)



# tests  ####
### read data   ####

test_that('read data',{


  data = read_xcsv(verbose = FALSE,
                   file = "https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")%>%
    is.data.frame() %>%
    expect_equal(TRUE)

})
test_that('read data - pooled',{

  pooled_data = read_xcsv(verbose = FALSE,
                          file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv")%>%
    is.data.frame() %>%
    expect_equal(TRUE)

})
test_that('read data - pooled ts',{

  pooled_data = read_xcsv(verbose = FALSE,
                          file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv")%>%
    check_ts(date_col = 'Week') %>%
    is.data.frame() %>%
    expect_equal(TRUE)

})


### seasonality ####

test_that('seasonality',{

  pooled_data = read_xcsv(verbose = FALSE,
                          file = "https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")%>%
    check_ts(date_col = 'date') %>%
    get_seasonality(date_col_name = 'date',
                    date_type = 'weekly starting') %>%
    is.data.frame() %>%
    expect_equal(TRUE)

})
test_that('seasonality - pooled',{

  pooled_data = read_xcsv(verbose = FALSE,
                          file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv")%>%
    check_ts(date_col = 'Week') %>%
    get_seasonality(date_col_name = 'Week',
                    pool_var = 'country',
                    date_type = 'weekly starting') %>%
    is.data.frame() %>%
    expect_equal(TRUE)

})


### run model   ####

test_that('run_model ivs, dv',{

  run_model(data = data, dv = dv, ivs = ivs) %>%
    class() %>%
    expect_equal('lm')

})

### next steps  ---------------------------------------------------------------

test_that("what next - output dataframe", {
  model %>% 
    what_next() %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that("what next - output not all na", {
  model %>% 
    what_next() %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(FALSE)
})

test_that("what next - output dataframe - diff FALSE - not pooled", {
  model %>% 
    what_next(r2_diff = FALSE) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that("what next - output not all na - diff FALSE - not pooled", {
  model %>% 
    what_next(r2_diff = FALSE) %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(FALSE)
})

test_that("what next - pooled - output dataframe", {
  pooled_model %>% 
    what_next() %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that("what next - pooled - output not all na", {
  pooled_model %>% 
    what_next() %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(FALSE)
})

test_that("what trans - output dataframe", {
  run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl')) %>%
    what_trans(variable = 'cyl',trans_df = data.frame(
    name = c('diminish', 'decay', 'lag', 'ma', 'log', 'hill', 'sin', 'exp'),
    func = c('linea::diminish(x,a)',
             'linea::decay(x,a)',
             'linea::lag(x,a)',
             'linea::ma(x,a)',
             'log(x,a)',
             "linea::hill_function(x,a,b,c)",
             'sin(x*a)',
             '(x^a)'),order = 1:8) %>%
      dplyr::mutate(val = '') %>%
      dplyr::mutate(val = dplyr::if_else(condition = name == 'hill',
                                         '(1,5,50),(1 ,5,50),(1,5,50)',
                                         val))) %>%
    is.data.frame() %>% 
    expect_equal(TRUE)
})
test_that("what trans - output not all na", {
  run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl')) %>%
  what_trans(variable = 'cyl',trans_df = data.frame(
  name = c('diminish', 'decay', 'lag', 'ma', 'log', 'hill', 'sin', 'exp'),
  func = c('linea::diminish(x,a)',
           'linea::decay(x,a)',
           'linea::lag(x,a)',
           'linea::ma(x,a)',
           'log(x,a)',
           "linea::hill_function(x,a,b,c)",
           'sin(x*a)',
           '(x^a)'),order = 1:8) %>%
    dplyr::mutate(val = '') %>%
    dplyr::mutate(val = dplyr::if_else(condition = name == 'hill',
                                       '(1,5,50),(1 ,5,50),(1,5,50)',
                                       val))) %>%
    is.na() %>%
    all() %>%
    expect_equal(FALSE)
})

test_that("what combo - output dataframe", {
  data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")
  dv = 'ecommerce'
  ivs = c('christmas','black.friday')
  combo_trans_df = data.frame(
    name = c('diminish', 'decay', 'hill', 'exp'),
    func = c(
      'linea::diminish(x,a)',
      'linea::decay(x,a)',
      "linea::hill_function(x,a,b,c)",
      '(x^a)'
    ),
    order = 1:4
  ) %>%
    dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'hill',
                                                 '(1,5,50),(1,5,50),( 1,5,50)',
                                                 '')) %>%
    dplyr::mutate(online_media = dplyr::if_else(condition = name == 'diminish',
                                                '.1,.5, 10 ',
                                                '')) %>%
    dplyr::mutate(online_media = dplyr::if_else(condition = name == 'decay',
                                                '.1,.7 ',
                                                online_media)) %>%
    dplyr::mutate(online_media = dplyr::if_else(condition = name == 'exp',
                                                '.5,2,3',
                                                online_media)) %>%
    dplyr::mutate(promo = '') %>% 
    {what_combo(trans_df = .,dv = dv,data = data)} %>% 
    {.[['results']]} %>% 
    is.data.frame() %>% 
    expect_equal(TRUE)
})
test_that("what combo - output not all na", {
  data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")
  dv = 'ecommerce'
  ivs = c('christmas','black.friday')
  combo_trans_df = data.frame(
    name = c('diminish', 'decay', 'hill', 'exp'),
    func = c(
      'linea::diminish(x,a)',
      'linea::decay(x,a)',
      "linea::hill_function(x,a,b,c)",
      '(x^a)'
    ),
    order = 1:4
  ) %>%
    dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'hill',
                                                 '(1,5,50),(1,5,50),( 1,5,50)',
                                                 '')) %>%
    dplyr::mutate(online_media = dplyr::if_else(condition = name == 'diminish',
                                                '.1,.5, 10 ',
                                                '')) %>%
    dplyr::mutate(online_media = dplyr::if_else(condition = name == 'decay',
                                                '.1,.7 ',
                                                online_media)) %>%
    dplyr::mutate(online_media = dplyr::if_else(condition = name == 'exp',
                                                '.5,2,3',
                                                online_media)) %>%
    dplyr::mutate(promo = '') %>% 
    {what_combo(trans_df = .,dv = dv,data = data)} %>% 
    {.[['results']]} %>% 
    is.na() %>%
    all() %>%
    expect_equal(FALSE)
})

### get gt      ------------------------------------------------------------------

test_that("gtrends_f - pooled - output dataframe",{
  gt_f(data = pooled_data,
       kw = 'bitcoin',
       date_col = pooled_id_var) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})


### resp curves ------------------------------------------------------------------

test_that("response curves - pooled - output is plotly",{

  pooled_model %>%
    response_curves() %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(TRUE)
})


test_that("response curves - not pooled - output is dataframe",{

  model %>%
    response_curves(table = TRUE) %>%
    is.data.frame() %>%
    expect_equal(TRUE)

})
