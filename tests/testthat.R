# libs   ----
library(testthat)
library(linea)
library(plotly)
library(tibble)
library(dplyr)
library(tidyr)
# library(zoo)
## GitHub Install test
# devtools::install_github("linea-analytics/linea")

# set up ####

color_palette = list(
    decomp_color_1 = "#636363",
    decomp_color_2 = "#143fba",
    decomp_color_3 = "#006a53",
    decomp_color_4 = "#209162",
    decomp_color_5 = "#33eea0",
    decomp_color_6 = "#8f9d35",
    decomp_color_7 = "#00cf74",
    decomp_color_8 = "#b400da",
    decomp_color_9 = "#fff200",
    decomp_color_10 = "#ff5c00",
    decomp_color_11 = "#6a000d",
    decomp_color_12 = "#ff006e",
    charts_font_color = '#1c0022',
    charts_background_color = "#FFFFFF",
    charts_grid_line_color = '#1c0022',
    charts_color_3 = "#C90F3A",
    charts_color_2 = "#00cf74",
    charts_color_1 = '#1c0022'
) %>% unlist() %>% as.character()
### NOT POOLED  ----

# import data
data = linea::sales_ts %>% 
  check_ts(verbose = TRUE,
          allow_non_num = TRUE,
          date_col = "week") %>%
  get_seasonality(verbose = FALSE,
                  date_col_name = "week",
                  date_type = "weekly starting")


# vars
dv = "sales"
ivs = c("relative_price","vod_spend", "dec", "mar")
id_var = "week"

# model table
model_table = build_model_table(ivs)
model_table$decay[2] = '0.5'
model_table$hill[2] = '20000,5'
model_table = model_table %>% get_variable_t()

# category
category = tibble(
  variable = ivs[-1],
  category = c("media", "seasonality","seasonality"),
  calc = c("","","")
)

# run model
model = run_model(
  verbose = TRUE,
  data = data,
  dv = dv,
  model_table = model_table,
  normalise_by_pool = FALSE,
  id_var = id_var 
)



### POOLED      ----

# import data
pooled_data = pooled_gt_data %>%
  check_ts(verbose = FALSE,
          allow_non_num = TRUE,
          date_col = "Week") %>%
  get_seasonality(verbose = FALSE,
                  date_col_name = "Week",
                  date_type = "weekly starting")

# vars
pooled_dv = "amazon"
pooled_ivs = c("rakhi", "christmas", "diwali")
pooled_id_var = "Week"
pool_var = 'country'

# model table
pooled_model_table = build_model_table(c(pooled_ivs, "", ""))

# category
pooled_category = data.frame(
  variable = c("rakhi" , "christmas"),
  category = c("a", "a"),
  calc = c("", "")
)

# run model
pooled_model = run_model(
  verbose = TRUE,
  data = pooled_data,
  dv = pooled_dv,
  id_var = pooled_id_var,
  pool_var =  pool_var,
  model_table = pooled_model_table,
  normalise_by_pool = TRUE
)

###


### DAILY       ####

daily_id_var = "date"

daily_data = linea::cran_downloads %>% 
  get_seasonality(date_col_name = daily_id_var,
                  date_type = "daily",
                  verbose = T) %>% 
  get_oecd_data(country_code = "GB",
                date_col_name = daily_id_var,
                verbose = T)

daily_categories = data.frame(
  variable = "GB_unemp",
  category= "economy",
  calc = "min"
)

daily_model = run_model(data = daily_data,
                        categories = daily_categories,
                        dv = "count",
                        ivs = c("weekend","weekday_Friday","trend","GB_unemp"))

# tests  ####
### read data   ####

# ADD "LOCAL"CSV FILES TO R-PACKAGE: https://r-pkgs.org/data.html
# test_that('read data',{
# 
#   data = read_xcsv(verbose = FALSE,
#                    file = "extdata/sales_ts.csv")%>%
#     is.data.frame() %>%
#     expect_equal(TRUE)
# 
# })
# test_that('read data - pooled',{

#   pooled_data = read_xcsv(verbose = FALSE,
#                           file = "extdata/pooled_gt_data.csv")%>%
#     is.data.frame() %>%
#     expect_equal(TRUE)

# })
# test_that('read data - pooled ts',{

#   pooled_data = read_xcsv(verbose = FALSE,
#                           file = "extdata/pooled_gt_data.csv")%>%
#     check_ts(date_col = 'Week') %>%
#     is.data.frame() %>%
#     expect_equal(TRUE)

# })


### seasonality ####

test_that('seasonality',{

  pooled_data = pooled_gt_data %>%
    check_ts(date_col = 'Week') %>%
    get_seasonality(date_col_name = 'Week',
                    date_type = 'weekly starting') %>%
    is.data.frame() %>%
    expect_equal(TRUE)

})
test_that('seasonality - pooled',{

  pooled_data = pooled_gt_data %>%
    check_ts(date_col = 'Week') %>%
    get_seasonality(date_col_name = 'Week',
                    pool_var = 'country',
                    date_type = 'weekly starting') %>%
    is.data.frame() %>%
    expect_equal(TRUE)

})
test_that('seasonality - daily',{
  
  daily_data = daily_data %>%
    check_ts(date_col = 'date') %>%
    get_seasonality(date_col_name = 'date') %>%
    is.data.frame() %>%
    expect_equal(TRUE)
  
})
test_that('seasonality - spot check',{
  
  daily_data = daily_data %>%
    check_ts(date_col = 'date') %>%
    get_seasonality(date_col_name = 'date') %>%
    pull(good_friday) %>% 
    sum() %>% 
    expect_equal(2)
  
})

### economy     ####

test_that('oecd - weekly',{
  
  data = sales_ts %>%
    get_oecd_data(
      date_col_name = 'week',
      country_code = 'US',
      date_type = "weekly starting"
    ) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
  
})
test_that('oecd - weekly ending',{
  
  data = sales_ts %>%
    get_oecd_data(
      date_col_name = 'week',
      country_code = 'US',
      date_type = "weekly ending"
    ) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
  
})
test_that('oecd - daily',{
  
  data = cran_downloads %>%
    get_oecd_data(
      date_col_name = 'date',
      country_code = 'US',
      date_type = "daily"
    ) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that('oecd - pooled',{
  
  data = pooled_gt_data %>%
    get_oecd_data(
      date_col_name = 'Week',
      country_code = 'GB',
      date_type = "weekly starting"
    ) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})

test_that('oecd - weekly - no `date_type`',{
  
  data = sales_ts %>%
    get_oecd_data(
      date_col_name = 'week',
      country_code = 'US',
    ) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
  
})
test_that('oecd - daily - no `date_type`',{
  
  data = cran_downloads %>%
    get_oecd_data(
      date_col_name = 'date',
      country_code = 'US',
    ) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that('oecd - pooled - no `date_type`',{
  
  data = pooled_gt_data %>%
    get_oecd_data(
      date_col_name = 'Week',
      country_code = 'GB'
    ) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})

test_that('oecd - weekly - zero NAs',{
  
  data = sales_ts %>%
    get_oecd_data(
      date_col_name = 'week',
      country_code = 'US',
      append = F
    ) %>%
    is.na() %>% 
    colSums() %>% 
    sum() %>% 
    expect_equal(0)
  
})
test_that('oecd - daily - zero NAs',{
  
  data = cran_downloads %>%
    get_oecd_data(
      date_col_name = 'date',
      country_code = 'US',
      append = F
    ) %>%
    is.na() %>% 
    colSums() %>% 
    sum() %>% 
    expect_equal(0)
  
})
test_that('oecd - pooled - zero NAs',{
  
  data = pooled_gt_data %>%
    get_oecd_data(
      date_col_name = 'Week',
      country_code = 'US',
      append = F
    ) %>%
    is.na() %>% 
    colSums() %>% 
    sum() %>% 
    expect_equal(0)
  
})

test_that('world bank - weekly',{
  
  data = sales_ts %>%
    check_ts(date_col = 'week') %>%
    get_economy(date_col_name = 'week',country_code = 'US') %>%
    is.data.frame() %>%
    expect_equal(TRUE)
  
})

### run model   ####

test_that('run_model ivs, dv',{

  run_model(data = data, dv = dv, ivs = ivs) %>%
    class() %>%
    expect_equal('lm')

})


test_that('run_model mt, dv - pooled',{
  
  run_model(data = pooled_data, dv = pooled_dv,model_table = pooled_model_table) %>%
    class() %>%
    expect_equal('lm')
  
})

### decomping   ####

test_that('decomp',{

  model %>% 
    decomp_chart() %>% 
    class() %>%
    {.[1]} %>% 
    expect_equal('plotly')

})
test_that('decomp - math',{
  
  temp_var_decomp = model$decomp_list$variable_decomp
  temp_coef = model$output_model_table %>% 
    select(variable,variable_t,coef) %>% 
    filter(variable == 'vod_spend') %>% 
    pull(coef)
  
  temp_v = linea::sales_ts$vod_spend
  temp_trans_df = default_trans_df()
  temp_trans_df$params = c('2e4,5','.5','0','0')
  
  temp_manual_decomp_val = vapply_transformation(v,trans_df) %>% 
    {. * temp_coef} %>% 
    sum()
  
  temp_decomp_val = temp_var_decomp %>% 
    filter(variable == 'vod_spend_hill_20000,5_decay_0.5') %>% 
    pull(contrib) %>% 
    sum()
  
  expect_equal(object = temp_decomp_val,
               expected = temp_manual_decomp_val)
  
})

test_that('decomp - pooled',{
  
  pooled_model %>% 
    decomp_chart(model = .,pool = 'UK',verbose = T) %>% 
    class() %>%
    {.[1]} %>% 
    expect_equal('plotly')
  
  
})
test_that('decomp - pooled - math',{
  
  # decomp to test
  temp_test_val = pooled_model$decomp_list$variable_decomp %>% 
    filter(variable == 'christmas') %>% 
    pull(contrib) %>% 
    sum() %>% 
    round(2)
  
  # manual decomp
  temp_coef = pooled_model$output_model_table %>% 
    select(variable,variable_t,coef) %>% 
    filter(variable == 'christmas') %>% 
    pull(coef)
  
  temp_manual_val = linea::pooled_gt_data %>% 
    left_join(y = model$pool_mean,by = c('country'='pool')) %>% 
    mutate(christmas = christmas * mean * !!temp_coef) %>% 
    pull(christmas) %>% 
    sum() %>% 
    round(2)
    
  expect_equal(object = temp_test_val,
               expected = temp_manual_val)
  
})

test_that('decomp - tails',{
  
  pooled_model %>%
    decomping(
      tail_window = 100,
      de_normalise = T
    ) %>%
    decomp_chart(decomp_list = ., pool = 'India') %>% 
    class() %>%
    {.[1]} %>%
    expect_equal('plotly')
  
})
test_that('decomp - colors',{
  
  model %>% 
    decomp_chart(
      actual_line_color = 'blue',
      resid_line_color = '#f00',
      plot_bgcolor = "rgba(255,255,255,0.2)", 
      paper_bgcolor = "black",
      font_color = 'white',
      zero_line_color = 'black'
    ) %>%
    class() %>%
    {.[1]} %>% 
    expect_equal('plotly')
  
})

### offset      ####

test_that('offset',{
  
  model_table %>%
    mutate(fixed = if_else(variable == 'dec', '3000', fixed)) %>% 
    mutate(fixed = if_else(variable == 'mar', '3000', fixed)) %>% 
    with(fixed) %>%
    as.numeric() %>% 
    {sum(.,na.rm = TRUE)>0} %>% 
    expect_equal(TRUE)
  
})

### next steps  ---------------------------------------------------------------

test_that("what next - output dataframe", {
  model %>%
    what_next(data = data) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that("what next - output not all na", {
  model %>%
    what_next(data = data) %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(FALSE)
})

test_that("what next - output dataframe - diff FALSE - not pooled", {
  model %>%
    what_next(r2_diff = FALSE,data = data) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that("what next - output not all na - diff FALSE - not pooled", {
  model %>%
    what_next(r2_diff = FALSE,data = data) %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(FALSE)
})


test_that("what next - pooled - test output reporducible in model", {
  
  # get test outputs
  output = pooled_model %>%
    what_next(data = pooled_data)
  
  # build new model based on test output
  new_model_table = pooled_model$model_table %>% 
    rbind(c(rep("",4),output$variable[1],"","","")) %>% 
    get_variable_t()
  
  new_model = run_model(data = pooled_data,
                        dv = pooled_dv,
                        model_table = new_model_table,
                        pool_var = pool_var,
                        normalise_by_pool = T)
  
  new_coefs = new_model$coefficients %>% 
    round(4)
  
  # check if results match
  expect_equal(new_coefs[[length(new_coefs)]],
               round(output$coef[1],4))
  
})
test_that("what next - pooled - output dataframe", {
  pooled_model %>%
    what_next(data = pooled_data) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that("what next - pooled - output not all na", {
  pooled_model %>%
    what_next(data = pooled_data) %>%
    select(-variable) %>%
    is.na() %>%
    all() %>%
    expect_equal(FALSE)
})

test_that("what trans - output dataframe", {
  run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl')) %>%
    what_trans(variable = 'cyl',trans_df = data.frame(
    name = c('diminish', 'decay', 'lag', 'ma', 'log', 'hill', 'sin', 'exp'),
    ts = c(FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
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
  ts = c(FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
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
test_that("what trans - pooled - test output reporducible in model", {
  
  # generate what_trans_df
  what_trans_df = data.frame(
    name = c('diminish', 'decay', 'lag', 'ma', 'log', 'hill', 'sin', 'exp'),
    ts = c(FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
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
                                       val)) 
  
  # get test outputs
  output = pooled_model %>%
    what_trans(data = pooled_data,
               trans_df = what_trans_df,
               variable = "trend")
  
  # build new model based on test output
  new_model_table = pooled_model$model_table %>% 
    rbind(c(paste0(output[1,1:3],collapse = ","),rep("",3),"trend","","","")) %>% 
    get_variable_t()
  
  new_model = run_model(data = pooled_data,
                        dv = pooled_dv,
                        model_table = new_model_table,
                        pool_var = pool_var,
                        normalise_by_pool = T)
  
  new_coefs = new_model$coefficients %>% 
    round(4)
  
  # check if results match
  expect_equal(new_coefs[[length(new_coefs)]],
               round(output$coef[1],4))
  
})


test_that("what combo - output dataframe", {
  data = linea::sales_ts
  dv = 'sales'
  ivs = c('dec','mar')
  combo_trans_df = data.frame(
    name = c('diminish', 'decay', 'hill', 'exp'),
    ts = c(FALSE,TRUE,FALSE,FALSE),
    func = c(
      'linea::diminish(x,a)',
      'linea::decay(x,a)',
      "linea::hill_function(x,a,b,c)",
      '(x^a)'
    ),
    order = 1:4
  ) %>%
    dplyr::mutate(vod_spend = dplyr::if_else(condition = name == 'hill',
                                                 '(1,5,50),(1,5,50),( 1,5,50)',
                                                 '')) %>%
    dplyr::mutate(display_spend = dplyr::if_else(condition = name == 'diminish',
                                                '.1,.5, 10 ',
                                                '')) %>%
    dplyr::mutate(display_spend = dplyr::if_else(condition = name == 'decay',
                                                '.1,.7 ',
                                                display_spend)) %>%
    dplyr::mutate(display_spend = dplyr::if_else(condition = name == 'exp',
                                                '.5,2,3',
                                                display_spend)) %>%
    {what_combo(trans_df = .,dv = dv,data = data)} %>%
    {.[['results']]} %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})
test_that("what combo - output not all na", {
  data = linea::sales_ts
  dv = 'sales'
  ivs = c('dec','mar')
  combo_trans_df = data.frame(
    name = c('diminish', 'decay', 'hill', 'exp'),
    ts = c(FALSE,TRUE,FALSE,FALSE),
    func = c(
      'linea::diminish(x,a)',
      'linea::decay(x,a)',
      "linea::hill_function(x,a,b,c)",
      '(x^a)'
    ),
    order = 1:4
  ) %>%
    dplyr::mutate(vod_spend = dplyr::if_else(condition = name == 'hill',
                                                 '(1,5,50),(1,5,50),( 1,5,50)',
                                                 '')) %>%
    dplyr::mutate(display_spend = dplyr::if_else(condition = name == 'diminish',
                                                '.1,.5, 10 ',
                                                '')) %>%
    dplyr::mutate(display_spend = dplyr::if_else(condition = name == 'decay',
                                                '.1,.7 ',
                                                display_spend)) %>%
    dplyr::mutate(display_spend = dplyr::if_else(condition = name == 'exp',
                                                '.5,2,3',
                                                display_spend)) %>%
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
       date_type = 'weekly starting',
       date_col = pooled_id_var) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})


test_that("gtrends_f - daily - output dataframe",{
  gt_f(data = daily_data,
       kw = 'bitcoin',
       date_type = "daily",
       date_col = daily_id_var
       ) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
})


### resp curves ------------------------------------------------------------------

test_that("response curves - weekly - output is plotly",{

  model %>%
    response_curves(colors = "viridis")  %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(TRUE)

})
test_that("response curves - weekly, points and histogram - output is plotly",{

  model %>%
    response_curves(points = TRUE,histogram = TRUE)  %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(TRUE)

})
test_that("response curves - weekly, table - output is dataframe",{

  model %>%
    response_curves(table = TRUE) %>%
    is.data.frame() %>%
    expect_equal(TRUE)

})

test_that("response curves - daily - output is plotly",{
  
  daily_model %>%
    response_curves(colors = "viridis")  %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(TRUE)
  
})
test_that("response curves - daily, points and histogram - output is plotly",{
  
  daily_model %>%
    response_curves(points = TRUE,histogram = TRUE)  %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(TRUE)
  
})
test_that("response curves - daily, table - output is dataframe",{
  
  daily_model %>%
    response_curves(table = TRUE) %>%
    is.data.frame() %>%
    expect_equal(TRUE)
  
})

test_that("response curves - pooled - output is plotly",{

  pooled_model %>%
    response_curves() %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(TRUE)
})
test_that("response curves - pooled, table - output is dataframe",{

  t = pooled_model %>%
    response_curves(table = TRUE) 
  
  t %>%
    class() %>%
    is.element(c("data.frame")) %>%
    expect_equal(TRUE)
})
test_that("response curves - pooled selected - output is plotly",{
  
  pooled_model %>%
    response_curves(pool = 'US') %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(TRUE)
})
test_that("response curves - pooled, points and histogram - output is plotly",{
  
  pooled_model %>%
    response_curves(points = TRUE, histogram = FALSE,x_max = 50,x_min = 0) %>%
    class() %>%
    is.element(c("plotly","htmlwidget")) %>%
    all() %>%
    expect_equal(TRUE)
  
})
