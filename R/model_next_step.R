#' what_next
#'
#' run model with other variables from the data
#'
#' Run a separate model for each numeric variable in the data provided.
#' Then, for each model run, return that model's fit and the variables' statistics.
#'
#' @param model Model object
#' @param data \code{data.frame} containing data for analysis
#' @param verbose A boolean to specify whether to print warnings
#' @param r2_diff A boolean to determine whether to add a column to compare new and original model R2
#' @importFrom purrr reduce
#' @importFrom methods is
#' @import tidyverse
#' @export
#' @return \code{data.frame} mapping variables' to the respective model's statistics.
#' @examples
#' run_model(
#'     data = mtcars,
#'     dv = 'mpg',
#'     ivs = c('disp', 'cyl')
#'   ) %>% 
#'   what_next()
what_next = function(model = NULL,
                     data = NULL,
                     verbose = FALSE,
                     r2_diff = TRUE) {

  # test    ####
  
  # model = run_model(data = mtcars,'mpg',ivs = 'cyl',save_all_raw_data = T)
  # data = NULL
  # verbose = FALSE
  # r2_diff = TRUE
  # 
  # model %>% what_next()

  # checks  ####
  
  # check verbose
  if (!is.logical(verbose)) {
    message("Warning: verbose provided mus be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  if (!is.logical(r2_diff)) {
    if (verbose)
      message("Warning: r2_diff provided mus be logical (TRUE or FALSE). Setting to TRUE.")
    r2_diff = TRUE
  }

  # check if model or model_table is provided is correct
  if (is.null(model)) {
    message("Error: No model provided. Returning NULL.")
    return(NULL)
  }
  if (!is(model,class2 = 'lm')) {
    message("Error: model must be of type 'lm'. Returning NULL.")
    return(NULL)
  }
  
  dv = model$dv
  model_table = model$model_table
  pool_var = model$pool_var
  id_var = model$id_var
  trans_df = model$trans_df
  normalise_by_pool = model$normalise_by_pool
  
  if (is.null(data)) {
    data = model$raw_data
  }

  ivs = model_table$variable %>% unique()
  test_ivs = colnames(data)
  test_ivs = test_ivs[!(test_ivs %in% c(ivs, dv, id_var, pool_var))]
  
  if(length(test_ivs)==0){
    message('Error: No new columns found in data. Returning NULL.')
    return(NULL)
  }
  
  # process ####

  #timer
  start_time = Sys.time()

  #1. get normalized value of non-test-vars
  if (normalise_by_pool) {
    norm_data = apply_normalisation(
      verbose = verbose,
      raw_data = data,
      pool_var = pool_var,
      dv = dv
    )
    if (normalise_by_pool & length(norm_data) == 2) {
      norm_data = norm_data$data
    }
  } else{
    norm_data = data
  }

  #2. get transformed value of non-test-vars
  trans_data = apply_transformation(
    model_table = model_table,
    raw_data = norm_data,
    trans_df = trans_df,
    verbose = verbose,
    pool_var = pool_var
  )

  model_vars_t = model_table %>%
    # replace raw var name with var_t
    get_variable_t(trans_df = trans_df) %>%
    pull(variable_t)

  # get starting model results
  m0_adj_R2 = summary(model)$adj.r.squared

  df = lapply(test_ivs, function(var) {
    # run model
    model = lm(data = trans_data,
               formula = build_formula(ivs = c(model_vars_t, var),
                                       dv = dv)) %>%
      TRY()

    # if model failed
    if (is.null(model)) {
      # fill row with empty
      return(c(var, NA, NA, NA))

    } else{
      # get model summary
      ms = summary(model)

      # remove back-tick
      rownames(ms$coefficients) = gsub(x =rownames(ms$coefficients),pattern = '`',replacement = '')

      # generate row
      coef = TRY(ms$coefficients[var, "Estimate"])
      if (is.null(coef)) {
        # fill row with empty
        return(c(var, NA, NA, NA))

      }

      adj_R2 = ms$adj.r.squared
      t_value = ms$coefficients[var, "t value"]

      return(c(var, adj_R2, t_value, coef))


    }
  })
  
  if(length(df)==1){
    df = purrr::reduce(df, rbind) %>%
      t() %>% 
      data.frame()
  } else if(length(df)>1){
    df = purrr::reduce(df, rbind) %>%
      data.frame()
  }


  df = df %>%
    tibble() %>%
    rename(
      variable = 1,
      adj_R2 = 2,
      t_stat = 3,
      coef = 4
    ) %>%
    arrange(desc(adj_R2)) %>%
    mutate(adj_R2 = as.numeric(adj_R2)) %>%
    mutate(t_stat = as.numeric(t_stat)) %>%
    mutate(coef = as.numeric(coef))

  if (r2_diff) {
    df = df %>%
      mutate(m0_adj_R2 = m0_adj_R2) %>%
      mutate(adj_R2_diff = (adj_R2 - m0_adj_R2) / m0_adj_R2) %>%
      select(-m0_adj_R2) %>%
      mutate()
  }

  # timer print
  if (verbose)
    print(Sys.time() - start_time)

  return(df)

}


#' get_vector_from_str
#'
#' get a numeric vector from string (e.g. '1,2,3')
#'
#' Get a numeric vector from string containing numbers separated by a separator (e.g. '1,2,3').
#'
#' @param string a string containing separated by a separator
#' @param sep a string representing the separator
#' @param zero a boolean defining whether the output vector must contain a zero
#' @importFrom magrittr '%>%'
#' @import stringr
#' @export
#' @return numeric vector from the string
#' @examples
#' get_vector_from_str('1,2,3')
#' get_vector_from_str('0.1')
#' get_vector_from_str('1;2;3',sep=';')
get_vector_from_str = function(string, sep = ',', zero = TRUE) {
  v = strsplit(x = string, split = sep)[[1]] %>%
    str_trim() %>%
    as.numeric()

  if (zero) {
    v = v %>%
      c(0)
  }

  v = v %>%
    unique()

  return(v[!is.na(v)])
}


#' what_trans
#'
#' run models with additional (transformed) variables from the data
#'
#' Run a separate model for each combination of transformations specified.
#' Then, for each model run, return that model's fit and the variables' statistics.
#'
#' @param model Model object
#' @param trans_df \code{data.frame}
#' @param variable string or character vector of variable names contained in raw_data \code{data.frame}
#' @param data \code{data.frame} containing data from analysis
#' @param r2_diff A boolean to determine whether to add a column to compare new and original model R2
#' @param verbose A boolean to specify whether to print warnings
#' @importFrom methods is
#' @importFrom stats lm
#' @import dplyr
#' @export
#' @return \code{data.frame} mapping variables' transformations to the respective model's statistics.
#' @examples
#' model = run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl'))
#'
#' trans_df = data.frame(
#'      name = c('diminish', 'decay', 'lag', 'ma', 'log', 'hill', 'sin', 'exp'),
#'      ts = c(FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
#'      func = c('linea::diminish(x,a)',
#'               'linea::decay(x,a)',
#'               'linea::lag(x,a)',
#'               'linea::ma(x,a)',
#'               'log(x,a)',
#'               "linea::hill_function(x,a,b,c)",
#'               'sin(x*a)',
#'               '(x^a)'),order = 1:8) %>%
#'   dplyr::mutate(val = '') %>%
#'   dplyr::mutate(val = dplyr::if_else(condition = name == 'hill',
#'                                      '(1,5,50),(1 ,5,50),(1,5,50)',
#'                                      val))
#'
#' variable = 'cyl'
#'
#' model %>%
#'  what_trans(variable = variable,trans_df = trans_df)
#'
what_trans = function(model = NULL,
                      trans_df = NULL,
                      variable = NULL,
                      data = NULL,
                      r2_diff = TRUE,
                      verbose = FALSE) {
  # tests   ####
  
  # model = run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl'))
  # 
  # trans_df = data.frame(
  #      name = c('diminish', 'decay', 'lag', 'ma', 'log', 'hill', 'sin', 'exp'),
  #      ts = c(FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE),
  #      func = c('linea::diminish(x,a)',
  #               'linea::decay(x,a)',
  #               'linea::lag(x,a)',
  #               'linea::ma(x,a)',
  #               'log(x,a)',
  #               "linea::hill_function(x,a,b,c)",
  #               'sin(x*a)',
  #               '(x^a)'),order = 1:8) %>%
  #   dplyr::mutate(val = '') %>%
  #   dplyr::mutate(val = dplyr::if_else(condition = name == 'hill',
  #                                      '(1,5,50),(1 ,5,50),(1,5,50)',
  #                                      val))
  # variable = 'cyl'
  # data = NULL
  # r2_diff = TRUE
  # verbose = FALSE
  
  # checks  ####

  # data = NULL
  # r2_diff = TRUE
  # verbose = FALSE

  if (!is.logical(verbose)) {
    message("Warning: verbose provided mus be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  if (!is.logical(r2_diff)) {
    if (verbose)
      message("Warning: r2_diff provided mus be logical (TRUE or FALSE). Setting to TRUE.")
    r2_diff = TRUE
  }
  if (is.null(trans_df)) {
    message("Error: trans_df must be provided. Returning NULL.")
    return(NULL)
  }
  if (is.null(variable)) {
    message("Error: variable must be provided. Returning NULL.")
    return(NULL)
  }


  # check if model or model_table is provided is correct
  if (is.null(model)) {
    if (is.null(model_table)) {
      message("Error: No model or model_table provided. Returning NULL.")
      return(NULL)
    }
  } else{
    if (!is(model,class2 = 'lm')) {
      message("Error: model must be of type 'lm'. Returning NULL.")
      return(NULL)
    }
    else{
      if (!('dv' %in% names(model))) {
        message("Error: model object must contain 'dv'. Returning NULL.")
        return(NULL)
      } else{
        dv = model$dv
      }

      if (!('model_table' %in% names(model))) {
        message("Error: model object must contain 'model_table'. Returning NULL.")
        return(NULL)
      } else{
        model_table = model$model_table
      }
    }
  }

  # check data
  if (is.null(data)) {
    data = model$raw_data
    if (is.null(data)) {
      message('Error: No data provided as data or in model.')
    }
  }

  # check pool
  if (model$normalise_by_pool) {
    
    groups = data %>%
      pull(!!sym(model$pool_var)) %>%
      unique()

    data = apply_normalisation(
      raw_data = data,
      pool_var = model$pool_var,
      dv = model$dv,
      verbose = verbose
    )

    # check norm_data
    if (length(data) == 2) {
      pool_mean = data$pool_mean
      data = data$data
    }

  }

  data = apply_transformation(
    raw_data = data,
    model_table = model$model_table,
    trans_df = model$trans_df,
    pool_var = model$pool_var,
    verbose = verbose
  )

  # process ####

  # clean trans_df
  trans_df = trans_df %>%
    check_trans_df() %>%
    mutate(val = gsub(
      pattern = ' ',
      replacement = '',
      x = val
    )) %>%
    filter(val != "")

  # split values by parameter (for func's with +2 params)
  ncols = max(stringr::str_count(trans_df$val, "\\),\\(")) + 1
  cols = letters[1:ncols]
  trans_df = tidyr::separate(
    fill = 'right',
    data = trans_df,
    col = 'val',
    into = cols,
    sep = "\\).\\("
  ) %>%
    zoo::na.fill('') %>%
    as.data.frame() %>%
    reshape2::melt(id.vars = c('name', 'order', 'func','ts'),
                   factorsAsStrings = FALSE) %>%
    filter(value != '')  %>%
    mutate(value = gsub(
      pattern = '\\(|\\)',
      replacement = '',
      x = value
    ))

  # split each parameter (to be tested)
  ncols = max(stringr::str_count(trans_df$value, ",")) + 1
  cols = paste0('param_', 1:ncols)
  trans_df = trans_df %>%
    tidyr::separate(
      fill = 'right',
      col = 'value',
      into = cols,
      sep = ","
    ) %>%
    zoo::na.fill('') %>%
    as.data.frame()

  # get all combinations
  df = lapply(1:nrow(trans_df), function(x) {
    v = trans_df[x, ] %>%
      as.vector()

    v = v[!(names(v) %in% c('name','order','func','ts','variable'))]
    v = as.numeric(v[v != ''])

    return(v)
  }) %>%
    expand.grid() %>%
    zoo::na.fill('') %>%
    as.data.frame()

  # rename combinations
  colnames(df) = paste0(trans_df$name, '_', trans_df$variable)

  # define output table to fill with loop
  df = cbind(df, tibble(
    adj_R2 = 0,
    t_stat = 0,
    coef = 0
  ))

  fs_name = trans_df %>% arrange(order, variable) %>% pull(name) %>% unique()
  fs = trans_df %>% arrange(order, variable) %>% pull(func) %>% unique()

  m = model$model_table[0, ]

  # for each combo
  for (i in 1:nrow(df)) {
    # i = 1

    var_t_name = variable
    data[, 'temp_var'] = data[, variable]


    # for each trans
    for (j in 1:length(fs_name)) {
      # j = 1

      f_name = fs_name[j]

      var_t_name = paste0(var_t_name, '_', f_name)
      # print(var_t_name)

      ps = trans_df %>%
        filter(name == f_name) %>%
        arrange(variable) %>%
        pull(variable)

      vals = c()

      e <- new.env()

      # for each param
      for (p in ps) {
        val = df[i, paste0(f_name, '_', p)]
        vals = c(vals, val)
        var_t_name = paste0(var_t_name, '_', val)
        # print(var_t_name)
        assign(p,val,envir = e)

      }

      m[1, f_name] = vals %>% paste0(collapse = ',')

      f = fs[j]


      if (model$normalise_by_pool) {
        for (g in groups) {
          # g=groups[1]
          x = data$temp_var[data[, model$pool_var] == g]
          x = f %>% run_text(env = e)
          data$temp_var[data[, model$pool_var] == g] = x

        }
      } else{
        x = data$temp_var
        x = f %>% run_text(env = e)
        data$temp_var = x
      }
    }

    data[, var_t_name] = data[, 'temp_var']
    data[, 'temp_var'] = NULL
    # print('_______________')

    m[1, 'variable'] = variable
    m[1, 'variable_t'] = var_t_name
    m = m %>%
      zoo::na.fill('') %>%
      as.data.frame() %>%
      bind_rows(model_table)

    ivs_t = m %>% select(variable_t)
    # build formula object
    formula = build_formula(dv = dv, ivs = ivs_t)# run model

    # print(var_t_name %in% colnames(data))

    model_temp = lm(formula = formula,
                    data = data) %>% TRY()


    # if model failed
    if (is.null(model_temp)) {
      # fill row with empty
      print(paste0(var_t_name, ' - NOPE'))
      # output[i, ] = list(iv, "0",0,0,0,0)

    } else{
      # get model summary
      ms = summary(model_temp)

      # generate row
      coef = ms$coefficients[var_t_name, "Estimate"] %>%
        TRY()
      if (is.null(coef)) {
        adj_R2 = 0
        t_stat = 0
        coef = 0

      } else{
        adj_R2 = ms$adj.r.squared
        t_stat = ms$coefficients[var_t_name, "t value"]
      }

      df$adj_R2[i] = adj_R2
      df$t_stat[i] = t_stat
      df$coef[i] = coef

    }
  }

  if (r2_diff) {
    m0_adj_R2 = summary(model)$adj.r.squared

    df = df %>%
      mutate(m0_adj_R2 = m0_adj_R2) %>%
      mutate(adj_R2_diff = (adj_R2 - m0_adj_R2) / m0_adj_R2) %>%
      select(-m0_adj_R2) %>%
      mutate()
  }

  return(df %>% arrange(-adj_R2))

}



#' combo_number
#'
#' The number of models from all combinations of transformations and variables
#'
#' Count the number of models from all combinations of transformations and variables
#'
#' @param model Model object
#' @param trans_df \code{data.frame} containing the transformations, variables and parameter values
#' @param verbose A boolean to specify whether to print warnings
#' @importFrom purrr discard
#' @importFrom methods is
#' @import dplyr
#' @export
#' @return numeric count of what_combo models
#' @examples
#'
#' # using a model object
#' data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")
#' dv = 'ecommerce'
#' ivs = c('christmas','black.friday')
#'
#'
#' trans_df = data.frame(
#'   name = c('diminish', 'decay', 'hill', 'exp'),
#'   ts = c(FALSE,TRUE,FALSE,FALSE),
#'   func = c(
#'     'linea::diminish(x,a)',
#'     'linea::decay(x,a)',
#'     "linea::hill_function(x,a,b,c)",
#'     '(x^a)'
#'   ),
#'   order = 1:4
#' ) %>%
#'   dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'hill',
#'                                                '(1,50),(1),(1,100)',
#'                                                '')) %>%
#'   dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'decay',
#'                                               '.1,.7 ',
#'                                               offline_media)) %>%
#'   dplyr::mutate(promo = '')
#'
#' model = run_model(data = data,dv = dv,ivs = ivs, trans_df = trans_df)
#'
#' combos = combo_number(model = model,trans_df = trans_df)
#'
#' #using the trans_df
#' combo_number(trans_df = trans_df)
combo_number = function(model = NULL,
                      trans_df = NULL,
                      verbose = FALSE) {
  # test    ####
  
  # raw_data = read_xcsv(verbose = FALSE,
  #                         file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv")
  # dv = "amazon"
  # ivs = c("rakhi", "diwali")
  # id_var = "Week"
  # pool_var = 'country'
  # 
  # model_table = build_model_table(c(ivs, "", ""))
  # model = run_model(
  #   id_var = id_var,
  #   verbose = FALSE,
  #   data = raw_data,
  #   dv = dv,
  #   pool_var = pool_var,
  #   model_table = model_table,
  #   normalise_by_pool = TRUE
  # )
  # 
  # wc_trans_df = model$trans_df %>% 
  #   mutate(christmas = if_else(name == 'decay','.1,.5,.9',''))
  # 
  # trans_df = wc_trans_df
  # 
  # model %>% what_combo(trans_df = trans_df)
  # 
  # data = NULL
  # dv = NULL
  # r2_diff = TRUE
  # return_model_objects = FALSE
  # verbose = FALSE
  
  # checks  ####
  
  # check logical
  if (!is.logical(verbose)) {
    message("Warning: verbose provided mus be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  # check trans_df (MUST HAVE)
  if (is.null(trans_df)) {
    message("Error: trans_df must be provided. Returning NULL.")
    return(NULL)
  }
  
  # check if model or dv and data are provided is correct
  if (is.null(model)) {
    if (is.null(dv) | is.null(data)) {
      message("Error: model or dv and data must be provided. Returning NULL.")
      return(NULL)
    } else{
      vars = colnames(trans_df)
      vars = vars[!(vars %in% c('name', 'order', 'func','ts'))]
      model = run_model(dv = dv,
                        data = data,
                        ivs = vars)
    }
  } else{
    if (!is(model,class2 = 'lm')) {
      message("Error: model must be of type 'lm'. Returning NULL.")
      return(NULL)
    }
  }
  
  model_table = model$model_table
  
  # process ####
  
  # clean trans_df
  trans_df = trans_df %>%
    check_trans_df() %>% 
    apply(2, function(x)
      gsub(' ', '', x)) %>%
    as.data.frame() %>%
    discard(~all(is.na(.) | . ==""))
  
  
  # get variables
  vars = colnames(trans_df)
  vars = vars[!(vars %in% c('name', 'order', 'func','ts'))]
  
  long_trans_df = list()
  
  for (var in vars) {
    # var = vars[1]
    
    ncols = max(stringr::str_count(trans_df[, var], "\\),\\(")) + 1
    cols = letters[1:ncols]
    temp_trans_df = tidyr::separate(
      fill = 'right',
      data = trans_df,
      col = var,
      into = cols,
      sep = "\\).\\("
    ) %>%
      zoo::na.fill('') %>%
      as.data.frame() %>%
      select(-vars[vars != var]) %>%
      reshape2::melt(id.vars = c('name', 'order', 'func','ts'),
                     factorsAsStrings = FALSE) %>%
      filter(value != '')  %>%
      mutate(value = gsub(
        pattern = '\\(|\\)',
        replacement = '',
        x = value
      )) %>%
      rename(parameter = variable) %>%
      mutate(variable = var)
    
    long_trans_df = append(long_trans_df, list(temp_trans_df))
  }
  
  long_trans_df = long_trans_df %>%
    Reduce(f = rbind)
  
  
  # split each parameter (to be tested)
  ncols = max(stringr::str_count(long_trans_df$value, ",")) + 1
  cols = paste0('param_', 1:ncols)
  long_trans_df = long_trans_df %>%
    tidyr::separate(
      fill = 'right',
      col = 'value',
      into = cols,
      sep = ","
    ) %>%
    zoo::na.fill('') %>%
    as.data.frame()
  
  
  
  # expand.grid for all combos of a single variable
  long_combo_df = list()
  
  for (var in vars) {
    # var = vars[1]
    temp_trans_df = long_trans_df %>%
      filter(variable == var)
    
    if (nrow(temp_trans_df) == 0) {
      next
    }
    
    col_names = paste0(temp_trans_df$name, '_', temp_trans_df$parameter)
    
    temp_trans_df = lapply(1:nrow(temp_trans_df), function(x) {
      v = temp_trans_df[x, ] %>%
        as.vector()
      
      v = v[!(names(v) %in% c('name','order','func','ts','parameter','variable'))]
      v = as.numeric(v[v != ''])
      
      return(v)
    }) %>%
      expand.grid() %>%
      zoo::na.fill('') %>%
      as.data.frame() %>%
      mutate(variable = var)
    
    colnames(temp_trans_df)[1:length(col_names)] = col_names
    
    long_combo_df =  append(long_combo_df, list(temp_trans_df))
    names(long_combo_df)[length(long_combo_df)] = var
  }
  
  # expand.grid for all combos across variables
  output_df = lapply(long_combo_df, function(x) {
    1:nrow(x)
  }) %>%
    expand.grid()
  
  n_rows = nrow(output_df)
  
  return(n_rows)
  
}

#' what_combo
#'
#' run models across combinations of transformations and variables
#'
#' Run a separate model for each combination of transformations specified.
#' The combinations are defined by the possible transformation parameters specified in the trans_df.
#' Then, for each model run, return that model's fit and the variables' statistics.
#'
#' @param model Model object
#' @param trans_df \code{data.frame} containing the transformations, variables and parameter values
#' @param data \code{data.frame} containing data from analysis
#' @param r2_diff A boolean to determine whether to add a column to compare new and original model R2
#' @param dv string specifying the dependent variable name
#' @param return_model_objects A boolean to specify whether to return model objects
#' @param verbose A boolean to specify whether to print warnings
#' @importFrom purrr discard
#' @importFrom methods is
#' @importFrom stats lm
#' @import dplyr
#' @export
#' @return list of two \code{data.frame} mapping variables' transformations to the respective model's statistics.
#' @examples
#'
#' # using a model object
#' data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")
#' dv = 'ecommerce'
#' ivs = c('christmas','black.friday')
#'
#'
#' trans_df = data.frame(
#'   name = c('diminish', 'decay', 'hill', 'exp'),
#'   ts = c(FALSE,TRUE,FALSE,FALSE),
#'   func = c(
#'     'linea::diminish(x,a)',
#'     'linea::decay(x,a)',
#'     "linea::hill_function(x,a,b,c)",
#'     '(x^a)'
#'   ),
#'   order = 1:4
#' ) %>%
#'   dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'hill',
#'                                                '(1,50),(1),(1,100)',
#'                                                '')) %>%
#'   dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'decay',
#'                                               '.1,.7 ',
#'                                               offline_media)) %>%
#'   dplyr::mutate(promo = '')
#'
#' model = run_model(data = data,dv = dv,ivs = ivs, trans_df = trans_df)
#'
#' combos = what_combo(model = model,trans_df = trans_df)
#'
#' #using the trans_df, data, and dv
#' what_combo(trans_df = trans_df, data = data, dv = dv)
what_combo = function(model = NULL,
                      trans_df = NULL,
                      data = NULL,
                      dv = NULL,
                      r2_diff = TRUE,
                      return_model_objects = FALSE,
                      verbose = FALSE) {

  # TODO
  # - trans_df columns
  # - check vars are in data
  # - dv in data
  
  # test    ####
  
  # raw_data = read_xcsv(verbose = FALSE,
  #                         file = "https://raw.githubusercontent.com/paladinic/data/main/pooled%20data.csv")
  # dv = "amazon"
  # ivs = c("rakhi", "diwali")
  # id_var = "Week"
  # pool_var = 'country'
  # 
  # model_table = build_model_table(c(ivs, "", ""))
  # model = run_model(
  #   id_var = id_var,
  #   verbose = FALSE,
  #   data = raw_data,
  #   dv = dv,
  #   pool_var = pool_var,
  #   model_table = model_table,
  #   normalise_by_pool = TRUE
  # )
  # 
  # wc_trans_df = model$trans_df %>% 
  #   mutate(christmas = if_else(name == 'decay','.1,.5,.9',''))
  # 
  # trans_df = wc_trans_df
  # 
  # model %>% what_combo(trans_df = trans_df)
  # 
  # data = NULL
  # dv = NULL
  # r2_diff = TRUE
  # return_model_objects = FALSE
  # verbose = FALSE
  
  # checks  ####

  # check logical
  if (!is.logical(verbose)) {
    message("Warning: verbose provided mus be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  if (!is.logical(r2_diff)) {
    if (verbose)
      message("Warning: r2_diff provided mus be logical (TRUE or FALSE). Setting to TRUE.")
    r2_diff = TRUE
  }
  if (!is.logical(return_model_objects)) {
    if (verbose)
      message("Warning: return_model_objects provided mus be logical (TRUE or FALSE). Setting to TRUE.")
    return_model_objects = TRUE
  }
  
  # check trans_df (MUST HAVE)
  if (is.null(trans_df)) {
    message("Error: trans_df must be provided. Returning NULL.")
    return(NULL)
  }

  # check if model or dv and data are provided is correct
  if (is.null(model)) {
    if (is.null(dv) | is.null(data)) {
      message("Error: model or dv and data must be provided. Returning NULL.")
      return(NULL)
    } else{
      model_null = TRUE
      vars = colnames(trans_df)
      vars = vars[!(vars %in% c('name', 'order', 'func','ts'))]
      model = run_model(dv = dv,
                        data = data,
                        ivs = vars)
    }
  } else{
    model_null = FALSE
    if (!is(model,class2 = 'lm')) {
      message("Error: model must be of type 'lm'. Returning NULL.")
      return(NULL)
    }
    else{
      dv = model$dv
      if (!is.null(dv)) {
        if (verbose) {
          message('Warning: replacing dv provided as argument with model dv.')
        }
      }
      if (is.null(data)) {
        data = model$raw_data
      }
    }
  }

  model_table = model$model_table

  # check pool
  if (model$normalise_by_pool) {
    
    groups = data %>%
      pull(!!sym(model$pool_var)) %>%
      unique()

    data = apply_normalisation(
      raw_data = data,
      pool_var = model$pool_var,
      dv = model$dv,
      verbose = verbose
    )

    # check norm_data
    if (length(data) == 2) {
      pool_mean = data$pool_mean
      data = data$data
    }

  }

  data = apply_transformation(
    raw_data = data,
    model_table = model$model_table,
    trans_df = model$trans_df,
    pool_var = model$pool_var,
    verbose = verbose
  )


  # process ####

  # clean trans_df
  trans_df = trans_df %>%
    check_trans_df() %>% 
    apply(2, function(x)
      gsub(' ', '', x)) %>%
    as.data.frame() %>%
    discard(~all(is.na(.) | . ==""))


  # get variables
  vars = colnames(trans_df)
  vars = vars[!(vars %in% c('name', 'order', 'func','ts'))]

  if(return_model_objects){
    model_list = list()
  }
  long_trans_df = list()

  for (var in vars) {
    # var = vars[1]

    ncols = max(stringr::str_count(trans_df[, var], "\\),\\(")) + 1
    cols = letters[1:ncols]
    temp_trans_df = tidyr::separate(
      fill = 'right',
      data = trans_df,
      col = var,
      into = cols,
      sep = "\\).\\("
    ) %>%
      zoo::na.fill('') %>%
      as.data.frame() %>%
      select(-vars[vars != var]) %>%
      reshape2::melt(id.vars = c('name', 'order', 'func','ts'),
                     factorsAsStrings = FALSE) %>%
      filter(value != '')  %>%
      mutate(value = gsub(
        pattern = '\\(|\\)',
        replacement = '',
        x = value
      )) %>%
      rename(parameter = variable) %>%
      mutate(variable = var)

    long_trans_df = append(long_trans_df, list(temp_trans_df))
  }

  long_trans_df = long_trans_df %>%
    Reduce(f = rbind)


  # split each parameter (to be tested)
  ncols = max(stringr::str_count(long_trans_df$value, ",")) + 1
  cols = paste0('param_', 1:ncols)
  long_trans_df = long_trans_df %>%
    tidyr::separate(
      fill = 'right',
      col = 'value',
      into = cols,
      sep = ","
    ) %>%
    zoo::na.fill('') %>%
    as.data.frame()



  # expand.grid for all combos of a single variable
  long_combo_df = list()

  for (var in vars) {
    # var = vars[1]
    temp_trans_df = long_trans_df %>%
      filter(variable == var)

    if (nrow(temp_trans_df) == 0) {
      next
    }

    col_names = paste0(temp_trans_df$name, '_', temp_trans_df$parameter)

    temp_trans_df = lapply(1:nrow(temp_trans_df), function(x) {
      v = temp_trans_df[x, ] %>%
        as.vector()

      v = v[!(names(v) %in% c('name','order','func','ts','parameter','variable'))]
      v = as.numeric(v[v != ''])

      return(v)
    }) %>%
      expand.grid() %>%
      zoo::na.fill('') %>%
      as.data.frame() %>%
      mutate(variable = var)

    colnames(temp_trans_df)[1:length(col_names)] = col_names

    long_combo_df =  append(long_combo_df, list(temp_trans_df))
    names(long_combo_df)[length(long_combo_df)] = var
  }

  # expand.grid for all combos across variables
  output_df = lapply(long_combo_df, function(x) {
    1:nrow(x)
  }) %>%
    expand.grid()

  # define output table to fill with loop
  output_df = cbind(output_df, tibble(adj_R2 = 0))

  # for each combo
  ## generate variable
  ## generate model
  for (i in 1:nrow(output_df)) {
    # i = 2

    m = model$model_table
    if(model_null){
      m = m[0,]
    }

    # for each var
    for (var in vars) {

      # var = vars[1]
      # print(paste0('var - ',var))

      var_t_name = var
      data[, 'temp_var'] = data[, var]

      temp_trans_df = long_trans_df[long_trans_df$variable == var, ]

      if (nrow(temp_trans_df) == 0) {
        next
      }

      fs_name = temp_trans_df %>% arrange(order) %>% pull(name) %>% unique()
      fs = temp_trans_df %>% arrange(order) %>% pull(func) %>% unique()


      # for each trans
      for (j in 1:length(fs_name)) {
        # j = 1

        # print(paste0('j - ',j))

        f_name = fs_name[j]

        ps = temp_trans_df %>%
          filter(name == f_name) %>%
          arrange(parameter) %>%
          pull(parameter)

        vals = c()

        e <- new.env()

        # for each param
        for (p in ps) {
          val = long_combo_df[[var]][output_df[i, var], paste0(f_name, '_', p)]
          vals = c(vals, val)
          # print(var_t_name)
          assign(p,val,envir = e)
        }

        var_t_name = paste0(var_t_name, '_', f_name, '_', paste0(vals,collapse = ','))

        f = fs[j]

        if (model$normalise_by_pool) {
          for (g in groups) {
            # g=groups[1]
            x = data$temp_var[data[, model$pool_var] == g]
            assign('x',x,envir = e)
            x = f %>% run_text(env = e)
            data$temp_var[data[, model$pool_var] == g] = x

          }
        } else{
          x = data$temp_var
          assign('x',x,envir = e)
          x = f %>% run_text(env = e)
          data$temp_var = x
        }
      }

      data[, var_t_name] = data[, 'temp_var']
      data[, 'temp_var'] = NULL
      # print('_______________')

      m = m %>%
        bind_rows(tibble(variable = var,
                         variable_t = var_t_name)) %>%
        zoo::na.fill('') %>%
        as.data.frame()

    }


    ivs_t = m %>% select(variable_t)
    # build formula object
    formula = build_formula(dv = model$dv, ivs = ivs_t)# run model

    # print(var_t_name %in% colnames(data))
    model_temp = lm(formula = formula,
                    data = data) %>% TRY()

    if(return_model_objects){
      model_list =  append(model_list,model_temp)
    }

    # print(model_temp)

    # if model failed
    if (is.null(model_temp)) {
      # fill row with empty
      row = list(0, 0, 0)

    } else{
      # get model summary
      ms = summary(model_temp) %>% 
        TRY()

      # drop back ticks
      # ...added because the var names have special characters
      rownames(ms$coefficients) = gsub(x =rownames(ms$coefficients),pattern = '`',replacement = '')

      # generate row
      if (is.null(ms)) {
        adj_R2 = 0

      } else{
        adj_R2 = ms$adj.r.squared
      }

      row = list(adj_R2)

      output_df[i,ncol(output_df)] = row

    }
  }
  # TODO: OPTIMISE above
  # - do not create variables more than once (check)
  # - ...also for partial transformations (e.g. x_t1, x_t1_t2, x_t1_t3)

  if (r2_diff) {
    m0_adj_R2 = summary(model)$adj.r.squared

    output_df = output_df %>%
      mutate(m0_adj_R2 = m0_adj_R2) %>%
      mutate(adj_R2_diff = (adj_R2 - m0_adj_R2) / m0_adj_R2) %>%
      select(-m0_adj_R2)
  }

  ordered = order(output_df$adj_R2,decreasing = T)

  output_df = output_df[ordered,]

  if(return_model_objects){

    model_list = model_list[ordered]

    combos = list(results = output_df,
                  trans_parameters = long_combo_df,
                  variables = vars,
                  long_trans_df = long_trans_df,
                  model_list = model_list,
                  model = model)
  }else{
    combos = list(results = output_df,
                  trans_parameters = long_combo_df,
                  long_trans_df = long_trans_df,
                  variables = vars,
                  model = model)
  }

  return(combos)

}

#' run_combo_model
#'
#' generate the mode object from the output of \code{linea::what_combo()}
#'
#' Generate the mode object from the output of \code{linea::what_combo()}
#' Using the specs from the output of \code{linea::what_combo()} a new model is run.
#'
#' @param combos output of \code{linea::what_combo()} function
#' @param results_row numeric value of the model (i.e. row from what_combo()$results) to run
#' @import dplyr
#' @export
#' @return list of two \code{data.frame} mapping variables' transformations to the respective model's statistics.
#' @examples
#'
#' # using a model object
#' data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")
#' dv = 'ecommerce'
#' ivs = c('christmas','black.friday')
#'
#'
#' trans_df = data.frame(
#'   name = c('diminish', 'decay', 'hill', 'exp'),
#'   ts = c(FALSE,TRUE,FALSE,FALSE),
#'   func = c(
#'     'linea::diminish(x,a)',
#'     'linea::decay(x,a)',
#'     "linea::hill_function(x,a,b,c)",
#'     '(x^a)'
#'   ),
#'   order = 1:4
#' ) %>%
#'   dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'hill',
#'                                                '(1,50),(1),(1,100)',
#'                                                '')) %>%
#'   dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'decay',
#'                                               '.1,.7 ',
#'                                               offline_media)) %>%
#'   dplyr::mutate(online_media = dplyr::if_else(condition = name == 'decay',
#'                                               '.1,.7 ',
#'                                               '')) %>%
#'   dplyr::mutate(promo = '')
#'
#' model = run_model(data = data,dv = dv,ivs = ivs, trans_df = trans_df)
#'
#' combos = what_combo(model = model,trans_df = trans_df)
#'
#' combos %>%
#'  run_combo_model()
run_combo_model = function(combos,
                           results_row = 1
                           ){
  # test    ####
  
  # results_row = 1
  # model_null = FALSE
  
  # check   ####
  
  model = combos$model
  res = combos$results
  input = res[results_row, ]
  vars = combos$variables
  params = combos$trans_parameters
  params_df = combos$long_trans_df

  model_table = model$model_table

  # if(model_null){
  #   model_table = model_table[0,]
  # }

  # for each var
  for (var in vars) {
    # var = vars[1]

    n = nrow(model_table) + 1
    model_table[n, ] = ''
    model_table[n, 'variable'] = var


    # get the parameters selected
    var_params = params[[var]][input[[var]], ]

    # get the trans applied
    df = params_df %>%
      filter(variable == var) %>%
      select(name, order, parameter)

    trans = df %>%
      pull(name) %>%
      unique()

    for (t in trans) {
      # t = trans[2]

      t_params = df %>%
        filter(name == t) %>%
        arrange(parameter) %>%
        mutate(col = paste0(name, '_', parameter)) %>%
        pull(col)

      param = var_params[t_params] %>%
        paste0(collapse = ',')

      model_table[n, t] = param

    }

  }

  model = model %>%
    re_run_model(model_table = model_table)

  return(model)
}
