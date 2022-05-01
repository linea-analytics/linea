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
#' @import tidyverse
#' @export
#' @examples
#' run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl')) %>% what_next()
what_next = function(model = NULL,
                     data = NULL,
                     verbose = F,
                     r2_diff = T) {
  # checks  ####
  
  # check verbose
  if (!is.logical(verbose)) {
    cat("Warning: verbose provided mus be logical (TRUE or FALSE). Setting to False. \n")
    verbose = F
  }
  if (!is.logical(r2_diff)) {
    if (verbose)
      cat("Warning: r2_diff provided mus be logical (TRUE or FALSE). Setting to TRUE. \n")
    r2_diff = T
  }
  
  # check if model or model_table is provided is correct
  if (is.null(model)) {
    cat("Error: no model provided. Returning NULL. \n")
    return(NULL)
  } else{
    if (class(model) != "lm") {
      cat("Error: model must be of type 'lm'. Returning NULL. \n")
      return(NULL)
    }
    else{
      if (!('dv' %in% names(model))) {
        cat("Error: model object must contain 'dv'. Returning NULL. \n")
        return(NULL)
      } else{
        dv = model$dv
      }
      
      if (!('model_table' %in% names(model))) {
        cat("Error: model object must contain 'model_table'. Returning NULL. \n")
        return(NULL)
      } else{
        model_table = model$model_table
      }
      
      if (!('meta_data' %in% names(model))) {
        cat("Warning: model object does not contain 'meta_data'. \n")
      }
      meta_data = model$meta_data
      
      if (!('id_var' %in% names(model))) {
        cat("Warning: model object does not contain 'id_var'.\n")
      }
      id_var = model$id_var
      
      if (!('trans_df' %in% names(model))) {
        cat("Warning: model object does not contain 'trans_df'. \n")
      }
      trans_df = model$trans_df
      
      if (!('normalise_by_pool' %in% names(model))) {
        if (verbose)
          cat("Warning: model object does not contain normalise_by_pool. Setting to FALSE. \n")
        normalise_by_pool = F
      } else{
        normalise_by_pool = model$normalise_by_pool
      }
      
    }
  }
  
  if (is.null(data)) {
    if (!('data' %in% names(model))) {
      cat(
        "Error: no data provided and model object does not contain 'data'. Returning NULL. \n"
      )
      return(NULL)
    } else{
      data = model$data
    }
  }
  
  # process ####
  
  #timer
  start_time = Sys.time()
  
  
  ivs = model_table$variable %>% unique()
  test_ivs = colnames(data)
  test_ivs = test_ivs[!(test_ivs %in% c(ivs, dv, id_var))]
  
  #1. get normalized value of non-test-vars
  if (normalise_by_pool) {
    norm_data = apply_normalisation(
      verbose = verbose,
      raw_data = data,
      # model_table =  model_table,
      meta_data = meta_data,
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
    data = norm_data,
    trans_df = trans_df,
    verbose = verbose,
    model_table = model_table,
    meta_data = meta_data
  )
  
  model_vars_t = model_table %>%
    # replace raw var name with var_t
    get_variable_t() %>%
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
  
  df = purrr::reduce(df, rbind) %>%
    data.frame() %>%
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
#' @examples
#' get_vector_from_str('1,2,3')
#' get_vector_from_str('0.1')
#' get_vector_from_str('1;2;3',sep=';')
get_vector_from_str = function(string, sep = ',', zero = T) {
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
#' @importFrom purrr reduce
#' @importFrom stats lm
#' @import dplyr
#' @export
#' @examples
#' run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl')) %>%
#' what_trans(variable = 'cyl',trans_df = data.frame(
#' name = c('diminish', 'decay', 'lag', 'ma', 'log', 'hill', 'sin', 'exp'),
#' func = c('linea::diminish(x,a)',
#'          'linea::decay(x,a)',
#'          'linea::lag(x,a)',
#'          'linea::ma(x,a)',
#'          'log(x,a)',
#'          "linea::hill_function(x,a,b,c)",
#'          'sin(x*a)',
#'          '(x^a)'),order = 1:8) %>%
#'   dplyr::mutate(val = '') %>%
#'   dplyr::mutate(val = dplyr::if_else(condition = name == 'hill',
#'                                      '(1,5,50),(1 ,5,50),(1,5,50)',
#'                                      val)))
what_trans = function(model = NULL,
                      trans_df = NULL,
                      variable = NULL,
                      data = NULL,
                      r2_diff = T,
                      verbose = F) {
  # checks  ####
  
  if (!is.logical(verbose)) {
    cat("Warning: verbose provided mus be logical (TRUE or FALSE). Setting to False. \n")
    verbose = F
  }
  if (!is.logical(r2_diff)) {
    if (verbose)
      cat("Warning: r2_diff provided mus be logical (TRUE or FALSE). Setting to TRUE. \n")
    r2_diff = T
  }
  if (is.null(trans_df)) {
    cat("Error: trans_df must be provided. Returning NULL. \n")
    return(NULL)
  }
  if (is.null(variable)) {
    cat("Error: variable must be provided. Returning NULL. \n")
    return(NULL)
  }
  
  
  # check if model or model_table is provided is correct
  if (is.null(model)) {
    if (is.null(model_table)) {
      cat("Error: no model or model_table provided. Returning NULL. \n")
      return(NULL)
    }
  } else{
    if (class(model) != "lm") {
      cat("\n Error: model must be of type 'lm'. Returning NULL.")
      return(NULL)
    }
    else{
      if (!('dv' %in% names(model))) {
        cat("\n Error: model object must contain 'dv'. Returning NULL.")
        return(NULL)
      } else{
        dv = model$dv
      }
      
      if (!('model_table' %in% names(model))) {
        cat("\n Error: model object must contain 'model_table'. Returning NULL.")
        return(NULL)
      } else{
        model_table = model$model_table
      }
    }
  }
  
  # check data
  if (is.null(data)) {
    data = model$data
    if (is.null(data)) {
      cat('Error: no data provided as data or in model. \n')
    }
  }
  
  # check pool
  if (model$normalise_by_pool) {
    meta_data = model$meta_data
    pool = meta_data$variable[toupper(meta_data$meta) == 'POOL']
    groups = data %>%
      pull(!!sym(pool)) %>%
      unique()
    
    data = apply_normalisation(
      raw_data = data,
      # model_table =  model$model_table,
      meta_data = model$meta_data,
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
    data = data,
    model_table = model$model_table,
    trans_df = model$trans_df,
    meta_data = model$meta_data,
    verbose = verbose
  )
  
  # process ####
  
  # clean trans_df
  trans_df = trans_df %>%
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
    reshape2::melt(id.vars = c('name', 'order', 'func'),
                   factorsAsStrings = F) %>%
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
    
    v = v[5:length(v)]
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
      # j = 2
      
      f_name = fs_name[j]
      
      var_t_name = paste0(var_t_name, '_', f_name)
      # print(var_t_name)
      
      ps = trans_df %>%
        filter(name == f_name) %>%
        arrange(variable) %>%
        pull(variable)
      
      vals = c()
      
      # for each param
      for (p in ps) {
        val = df[i, paste0(f_name, '_', p)]
        vals = c(vals, val)
        var_t_name = paste0(var_t_name, '_', val)
        # print(var_t_name)
        assign(p, val)
      }
      
      m[1, f_name] = vals %>% paste0(collapse = ',')
      
      f = fs[j]
      
      
      if (model$normalise_by_pool) {
        for (g in groups) {
          # g=groups[1]
          x = data$temp_var[data[, pool] == g]
          x = f %>% run_text(env = environment())
          data$temp_var[data[, pool] == g] = x
          
        }
      } else{
        x = data$temp_var
        x = f %>% run_text(env = environment())
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
#' @param verbose A boolean to specify whether to print warnings
#' @importFrom purrr reduce discard
#' @importFrom stats lm
#' @import dplyr
#' @export
#' @examples
#'
#' # using a model object
#' data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")
#' dv = 'ecommerce'
#' ivs = c('christmas','black.friday')
#'
#' model = run_model(data = data,dv = dv,ivs = ivs)
#'
#' trans_df = data.frame(
#'   name = c('diminish', 'decay', 'hill', 'exp'),
#'   func = c(
#'     'linea::diminish(x,a)',
#'     'linea::decay(x,a)',
#'     "linea::hill_function(x,a,b,c)",
#'     '(x^a)'
#'   ),
#'   order = 1:4
#' ) %>%
#'   dplyr::mutate(offline_media = dplyr::if_else(condition = name == 'hill',
#'                                                '(1,5,50),(1,5,50),( 1,5,50)',
#'                                                '')) %>%
#'   dplyr::mutate(online_media = dplyr::if_else(condition = name == 'diminish',
#'                                               '.1,.5, 10 ',
#'                                               '')) %>%
#'   dplyr::mutate(online_media = dplyr::if_else(condition = name == 'decay',
#'                                               '.1,.7 ',
#'                                               online_media)) %>%
#'   dplyr::mutate(online_media = dplyr::if_else(condition = name == 'exp',
#'                                               '.5,2,3',
#'                                               online_media)) %>%
#'   dplyr::mutate(promo = '')
#'
#' what_combo(model = model,trans_df = trans_df)
#'
#' #using the trans_df, data, and dv
#' what_combo(trans_df = trans_df, data = data, dv = dv)

what_combo = function(model = NULL,
                      trans_df = NULL,
                      data = NULL,
                      dv = NULL,
                      r2_diff = T,
                      verbose = F) {
  
  # TODO
  # - trans_df columns
  # - check vars are in data
  # - dv in data
  
  # checks  ####
  
  if (!is.logical(verbose)) {
    cat("Warning: verbose provided mus be logical (TRUE or FALSE). Setting to False. \n")
    verbose = F
  }
  if (!is.logical(r2_diff)) {
    if (verbose)
      cat("Warning: r2_diff provided mus be logical (TRUE or FALSE). Setting to TRUE. \n")
    r2_diff = T
  }
  if (is.null(trans_df)) {
    cat("Error: trans_df must be provided. Returning NULL. \n")
    return(NULL)
  }
  
  # check if model or dv and data are provided is correct
  if (is.null(model)) {
    if (is.null(dv) | is.null(data)) {
      cat("Error: model or dv and data must be provided. Returning NULL. \n")
      return(NULL)
    } else{
      vars = colnames(trans_df)
      vars = vars[!(vars %in% c('name', 'order', 'func'))]
      model = run_model(dv = dv,
                        data = data,
                        ivs = vars)
    }
  } else{
    if (class(model) != "lm") {
      cat("Error: model must be of type 'lm'. Returning NULL. \n")
      return(NULL)
    }
    else{
      if (!is.null(dv)) {
        if (verbose) {
          cat('Warning: replacing dv provided as argument with model dv. \n')
        }
      }
      if (!is.null(data)) {
        if (verbose) {
          cat('Warning: replacing data provided as argument with model data. \n')
        }
      }
    }
  }
  
  dv = model$dv
  model_table = model$model_table
  data = model$data
  
  # check pool
  if (model$normalise_by_pool) {
    meta_data = model$meta_data
    pool = meta_data$variable[toupper(meta_data$meta) == 'POOL']
    groups = data %>%
      pull(!!sym(pool)) %>%
      unique()
    
    data = apply_normalisation(
      raw_data = data,
      # model_table =  model$model_table,
      meta_data = model$meta_data,
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
    data = data,
    model_table = model$model_table,
    trans_df = model$trans_df,
    meta_data = model$meta_data,
    verbose = verbose
  )
  
  
  # process ####
  
  # clean trans_df
  trans_df = trans_df %>%
    apply(2, function(x)
      gsub(' ', '', x)) %>%
    as.data.frame() %>%
    discard(~all(is.na(.) | . ==""))
  
  
  # get variables
  vars = colnames(trans_df)
  vars = vars[!(vars %in% c('name', 'order', 'func'))]
  
  
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
      reshape2::melt(id.vars = c('name', 'order', 'func'),
                     factorsAsStrings = F) %>%
      filter(value != '')  %>%
      mutate(value = gsub(
        pattern = '\\(|\\)',
        replacement = '',
        x = value
      )) %>%
      rename(parameter = variable) %>%
      mutate(variable = var)
    
    long_trans_df = rlist::list.append(long_trans_df, temp_trans_df)
  }
  
  long_trans_df = long_trans_df %>%
    purrr::reduce(rbind)
  
  
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
    temp_trans_df = long_trans_df %>%
      filter(variable == var)
    
    if (nrow(temp_trans_df) == 0) {
      next
    }
    
    col_names = paste0(temp_trans_df$name, '_', temp_trans_df$parameter)
    
    temp_trans_df = lapply(1:nrow(temp_trans_df), function(x) {
      v = temp_trans_df[x, ] %>%
        as.vector()
      
      v = v[5:(length(v) - 1)]
      v = as.numeric(v[v != ''])
      
      return(v)
    }) %>%
      expand.grid() %>%
      zoo::na.fill('') %>%
      as.data.frame() %>%
      mutate(variable = var)
    
    colnames(temp_trans_df)[1:length(col_names)] = col_names
    
    long_combo_df = rlist::list.append(long_combo_df, temp_trans_df)
    names(long_combo_df)[length(long_combo_df)] = var
  }
  
  
  # expand.grid for all combos across variables
  output_df = sapply(long_combo_df, function(x) {
    1:nrow(x)
  }) %>%
    expand.grid()
  
  # define output table to fill with loop
  output_df = cbind(output_df, tibble(
    adj_R2 = 0,
    t_stat = 0,
    coef = 0
  ))
  
  
  # for each combo
  ## generate variable
  ## generate model
  for (i in 1:nrow(output_df)) {
    # i = 1
    
    # print(paste0('i - ',i))
    
    m = model$model_table
    
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
        
        var_t_name = paste0(var_t_name, '_', f_name)
        # print(var_t_name)
        
        
        ps = temp_trans_df %>%
          filter(name == f_name) %>%
          arrange(parameter) %>%
          pull(parameter)
        
        vals = c()
        
        
        # for each param
        for (p in ps) {
          val = long_combo_df[[var]][output_df[i, var], paste0(f_name, '_', p)]
          vals = c(vals, val)
          var_t_name = paste0(var_t_name, '_', val)
          # print(var_t_name)
          assign(p, val)
        }
        
        
        f = fs[j]
        
        
        if (model$normalise_by_pool) {
          for (g in groups) {
            # g=groups[1]
            x = data$temp_var[data[, pool] == g]
            x = f %>% run_text(env = environment())
            data$temp_var[data[, pool] == g] = x
            
          }
        } else{
          x = data$temp_var
          x = f %>% run_text(env = environment())
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
    
    # print(model_temp)
    
    # if model failed
    if (is.null(model_temp)) {
      # fill row with empty
      row = list(0, 0, 0)
      
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
      
      row = list(adj_R2, t_stat, coef)
      
      output_df[i, (ncol(output_df) - 2):(ncol(output_df))] = row
      
    }
  }
  # TO OPTIMISE above
  # - do not create variables more than once (check)
  # - ...also for partial transformations (e.g. x_t1, x_t1_t2, x_t1_t3)
  
  
  if (r2_diff) {
    m0_adj_R2 = summary(model)$adj.r.squared
    
    output_df = output_df %>%
      mutate(m0_adj_R2 = m0_adj_R2) %>%
      mutate(adj_R2_diff = (adj_R2 - m0_adj_R2) / m0_adj_R2) %>%
      select(-m0_adj_R2)
  }
  
  output_df = output_df %>%
    arrange(-adj_R2)
  
  return(list(results = output_df,
              trans_parameters = long_combo_df))
  
}
