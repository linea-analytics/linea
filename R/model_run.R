#' apply_normalisation
#'
#' Normalise data based on pool mean
#'
#' Normalise data by dividing all values in each pool by that pool's mean
#'
#' @export
#' @import tidyverse
#' @importFrom stats complete.cases
#' @param raw_data \code{data.frame} containing data for analysis
#' @param verbose A boolean to specify whether to print warnings
#' @param dv string specifying the dependent variable name
#' @param pool_var string specifying the pool variable name (e.g. 'country')
#' @return \code{list} containing a \code{tibble} of normalised data and a \code{tibble} of pool means
#' @examples
#' pooled_data = pooled_gt_data
#'
#' norm_data = apply_normalisation(
#'  raw_data = pooled_data,
#'  pool_var = 'country',
#'  dv = 'amazon')
apply_normalisation = function(raw_data = NULL,
                               pool_var = NULL,
                               dv = NULL,
                               verbose = FALSE){

  # checks  ####

  if(!is.logical(verbose)){
    message("Warning: verbose provided must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }

  # check raw data
  if (is.null(raw_data)) {
    message("Error: No raw_data provided. Returning NULL.")
    return(NULL)
  }
  if(!is.data.frame(raw_data)){
    message("Error: raw_data must be a data.frame. Returning NULL.")
    return(NULL)
  }

  # check dv
  if(is.null(dv)){
    message("Error: No dv provided. Returning raw_data")
    return(raw_data)
  }
  if(!(dv %in% colnames(raw_data))){
    message("Error: dv provided not found in raw_data. Returning raw_data")
    return(raw_data)
  }

  # check pool_var
  if(is.null(pool_var)){
    message("Error: No pool_var provided. Returning original raw_data.")
    return(raw_data)
  }
  if(!(pool_var %in% colnames(raw_data))){
    message("Error: pool variable not found in raw_data. Returning original raw_data.")
    return(raw_data)
  }

  # check for NAs
  if(any(!complete.cases(raw_data))) {
    if (verbose) {
      message("- Warning: NA's found in raw data will be replaced with zeros.")
    }
    raw_data[raw_data %>% is.na()] = 0
  }
  # if(any(complete.cases(raw_data))) {
  #   if(verbose){
  #     message("Warning: NA's found in raw_data will be dropped.")
  #   }
  #   raw_data = raw_data[complete.cases(raw_data), ]
  # }

  # process ####
  pool_mean = raw_data %>%
    group_by(!!sym(pool_var)) %>%
    summarise(mean = mean(!!sym(dv))) %>%
    rename(pool = !!sym(pool_var))

  # normalize dv by pool
  raw_data = raw_data  %>%
    group_by(!!sym(pool_var)) %>%
    mutate(temp_dv_var = !!sym(dv)) %>%
    mutate(temp_dv_var = temp_dv_var / mean(temp_dv_var, na.rm = TRUE)) %>%
    mutate(temp_dv_var = if_else(is.na(temp_dv_var), 0, temp_dv_var))
  raw_data[dv] = raw_data %>%
    pull(temp_dv_var)
  raw_data = raw_data %>%
    select(-temp_dv_var)

  return(list(data = raw_data,pool_mean = pool_mean))
}


#' #' apply_transformation
#'
#' Transform data based on model table
#'
#' Transform data based on the model table by applying the transformation functions (e.g. \code{decay}, \code{diminish}, \code{lag}, and \code{ma}) with the specified parameters to the respective variable
#'
#' @export
#' @param raw_data \code{data.frame} containing data for analysis
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply
#' @param model_table \code{data.frame} as created in the \code{build_model_table} function
#' @param pool_var string specifying the pool variable name (e.g. 'country')
#' @param verbose A boolean to specify whether to print warnings
#' @return \code{data.frame} of raw_data with added transformed variables
#' @examples
#'
#' pooled_data = pooled_gt_data
#'
#'
#' model_table = build_model_table('christmas')
#' model_table['decay'] = '0.5'
#'
#' trans_data = apply_transformation(
#'  raw_data = pooled_data,
#'  model_table = model_table,
#'  pool_var = 'country')
#'
apply_transformation = function(raw_data = NULL,
                                model_table = NULL,
                                trans_df = NULL,
                                pool_var = NULL,
                                verbose = FALSE) {
  # checks  #####

  # check verbose
  if(!is.logical(verbose)){
    message("Warning: verbose provided must be logical (TRUE or FALSE). Setting to FALSE.")
    verbose = FALSE
  }
  
  if(verbose){
    message("Applying transformations...")
  }

  # check model table provided (not NULL)
  if(is.null(raw_data)){
    if(verbose){
      message("- Error: No data provided for transformations. Returning NULL.")
    }
    return(NULL)
  }
  if(!is.data.frame(raw_data)){
    if(verbose){
      message("- Error: raw_data provided must be a data.frame. Returning NULL.")
    }
    return(NULL)
  }

  # check model_table provided (not NULL)
  if(is.null(model_table)){
    if(verbose){
      message("- Warning: No model_table provided for transformations. Returning raw_data.")
    }
    return(raw_data)
  }
  if(!is.data.frame(model_table)){
    if(verbose){
      message("- Warning: model_table provided must be a data.frame. Returning raw_data.")
    }
    return(raw_data)
  }
  else{
    if(!("variable" %in% colnames(model_table))){
      if(verbose){
        message("- Warning: model_table provided must contain a column called 'variable'. Returning raw_data.")
      }
      return(raw_data)
    }
  }

  # check trans_df
  if(is.null(trans_df)){
    if(verbose){
      message("= Info: No trans_df provided. Setting default trans_df.")
    }
    trans_df = default_trans_df()
  }
  if(!is.data.frame(trans_df)){
    if(verbose){
      message("- Warning: trans_df provided must be a data.frame. Setting default trans_df.")
    }
    trans_df = default_trans_df()
  }

  # check pool_var
  if(is.null(pool_var)){

    if (verbose) {
      message("- Info: No pool_var provided. A new `total_pool` variable will be generated.")
    }
    pool_var = 'total_pool'
    raw_data[pool_var] = pool_var

  }
  if(!(pool_var %in% colnames(raw_data))){
    message("- Info: pool variable not found in raw_data. A new `total_pool` variable will be generated.")
    pool_var = 'total_pool'
    raw_data[pool_var] = pool_var
  }

  # get variables from model table
  model_table = model_table %>%
    get_variable_t(trans_df = trans_df)

  variable_t = model_table$variable_t
  variable = model_table$variable
  trans = trans_df$name

  # process ####

  # for each variable...
  for(i in 1:length(variable)){

    var_t_name = variable_t[i]
    var_name = variable[i]

    # skip if blank
    if(var_name == ""){
      next
    }

    raw_data[,'temp_var'] = raw_data[,var_name]

    # for each transformation on trans_df
    for(j in 1:nrow(trans_df)){
      t_name = trans_df$name[j]
      t_func = trans_df$func[j]

      param_values = model_table %>%
        filter(variable_t == var_t_name) %>%
        pull(!!sym(t_name)) %>%
        as.character() %>%
        strsplit(split = '[,]') %>%
        unlist() %>%
        as.numeric()

      if (all(param_values == '')) {
        next
      }

      e <- new.env()

      for (i in 1:length(param_values)) {
        p_val = param_values[i]
        p_name = letters[i]

        assign(p_name,p_val,envir = e)

      }

      groups = raw_data %>%
        pull(!!sym(pool_var)) %>%
        unique()

      for(g in groups){

        x = raw_data$temp_var[raw_data[,pool_var]==g]
        x = t_func %>% run_text(env = e)
        raw_data$temp_var[raw_data[,pool_var]==g] = x

      }
    }

    raw_data[,var_t_name] = raw_data[,'temp_var']
    raw_data[,'temp_var'] = NULL
  }

  return(raw_data)

}

#' vapply_transformation
#'
#' Transform vector based on transformation parameters
#'
#' Transform vector based on the transformation parameters of the trans_df
#'
#' @export
#' @param v Numeric vector to be transformed
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply
#' @param verbose A boolean to specify whether to print warnings
#' @return Transformed numeric vector
#' @example 
#' v = linea::sales_ts$vod_spend
#' trans_df = linea::default_trans_df()
#' trans_df$params = c('2e4,5','.5','0','0')
#' linea::vapply_transformation(v,trans_df)
#' 
vapply_transformation = function(v,trans_df = NULL,verbose = FALSE){
  # tests   ----
  
  # v = linea::sales_ts$vod_spend
  # trans_df = linea::default_trans_df()
  # trans_df$params = c('2e4,5','.5','0','0')
  # verbose = TRUE
  
  # checks  ####

  # check verbose
  if (!is.logical(verbose)) {
    message("Warning: verbose provided must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }

  # check trans_df
  if (is.null(trans_df)) {
    if (verbose) {
      message("Warning: No trans_df provided. Setting default trans_df.")
    }
    trans_df = default_trans_df() %>%
      mutate(params = '')
  }


  # process ####
  for (j in 1:nrow(trans_df)) {
    t_name = trans_df$name[j]
    t_func = trans_df$func[j]
    t_vals = trans_df$params[j] %>%
      strsplit(split = "[,]") %>%
      unlist() %>%
      as.numeric()

    if (all(t_vals == '')) {
      next
    }

    e <- new.env()

    for (i in 1:length(t_vals)) {
      p_val = t_vals[i]
      p_name = letters[i]

      assign(p_name,p_val,envir = e)

    }

    x = v
    x = t_func %>% run_text(env = e)
    v = x

  }

  return(v)
}

#' run_model
#'
#' Run a linear regression model
#'
#' Run a linear regression model that captures the transformations applied in the \code{model_table} and the normalisation based on the \code{pool_var}.
#' A model can be run also by only supplying a dependent variable name \code{dv}, a vector of independent variable names dependent variable \code{ivs}, and the data that contains these.
#'
#' @export
#' @param data \code{data.frame} containing variables included in the model specification
#' @param dv string of the dependent variable name
#' @param ivs character vector of the independent variables names
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply
#' @param id_var string of id variable name (e.g. "date")
#' @param id_var_type string of id variable type (e.g. "weekly starting")
#' @param model_table \code{data.frame} as created in the \code{build_model_table} function
#' @param verbose A boolean to specify whether to print warnings
#' @param normalise_by_pool A boolean to specify whether to apply the normalisation
#' @param pool_var string specifying the pool variable name (e.g. 'country')
#' @param save_all_raw_data A boolean to specify whether to save whole input data to the model object
#' @param decompose A boolean to specify whether to generate the model decomposition
#' @param tail_window for time series, length of tail in decomposition
#' @param categories \code{data.frame} mapping variables to groups
#' @param colors named list of colors
#' @param dark_mode A boolean to specify charts should be in dark mode
#' @import tidyverse
#' @import tibble
#' @import zoo
#' @importFrom stats lm na.omit
#' @importFrom car vif
#' @return Model object
#' @examples
#'
#' trans_df = data.frame(
#'     name = c('diminish', 'decay', 'hill', 'exp'),
#'     func = c(
#'       'linea::diminish(x,a)',
#'       'linea::decay(x,a)',
#'       "linea::hill_function(x,a,b,c)",
#'       '(x^a)'
#'     ),
#'     order = 1:4
#' )
#'
#' data = linea::sales_ts
#' dv = 'sales'
#' ivs = c('dec','mar')
#'
#' run_model(data = data,
#'           dv = dv,
#'           ivs = ivs,
#'           trans_df = trans_df)
#'
#' run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl'))
#'
run_model = function(data = NULL,
                     dv = NULL,
                     ivs = NULL,
                     trans_df = NULL,
                     id_var = NULL,
                     id_format = NULL,
                     pool_var = NULL,
                     model_table = NULL,
                     verbose = FALSE,
                     normalise_by_pool = FALSE,
                     save_all_raw_data = TRUE,
                     decompose = TRUE,
                     tail_window = NULL,
                     categories = NULL,
                     colors = NULL,
                     dark_mode = NULL) {
  # test    ####

  # data = linea::sales_ts
  # dv = 'sales'
  # ivs = c('dec')
  # pool_var = NULL
  # trans_df = NULL
  # id_var = NULL
  # model_table = NULL
  # verbose = T
  # tail_window = NULL
  # normalise_by_pool = FALSE
  # save_all_raw_data = TRUE
  # decompose = TRUE
  # categories = NULL
  # colors = NULL

  # data = mtcars
  # dv = 'mpg'
  # ivs = c('wt','cyl','disp')
  # pool_var = NULL
  # trans_df = NULL
  # id_var = NULL
  # model_table = build_model_table(ivs = ivs) %>%
  #   mutate(hill = if_else(variable == 'wt','3,3','')) %>% 
  #   mutate(decay = if_else(variable == 'wt','.3',''))
  # verbose = T
  # tail_window = NULL
  # normalise_by_pool = FALSE
  # save_all_raw_data = TRUE
  # decompose = TRUE
  # categories = NULL
  # colors = NULL
  
  # data = linea::sales_ts
  # dv = 'sales'
  # ivs = c('dec','mar')
  # verbose = T
  # tail_window = NULL
  # normalise_by_pool = FALSE
  # save_all_raw_data = TRUE
  # decompose = TRUE
  # categories = NULL
  # model_table = NULL
  # pool_var = NULL
  # id_var = NULL
  # 
  # trans_df = data.frame(
  #   name = c('diminish', 'decay', 'hill', 'exp'),
  #   ts = c(FALSE,TRUE,FALSE,FALSE),
  #   func = c(
  #     'linea::diminish(x,a)',
  #     'linea::decay(x,a)',
  #     "linea::hill_function(x,a,b,c)",
  #     '(x^a)'
  #   ),
  #   order = 1:4
  # ) %>%
  #   dplyr::mutate(display_spend = dplyr::if_else(condition = name == 'hill',
  #                                                '(1,50),(1),(1,100)',
  #                                                '')) %>%
  #   dplyr::mutate(display_spend = dplyr::if_else(condition = name == 'decay',
  #                                               '.1,.7 ',
  #                                               display_spend)) %>%
  #   dplyr::mutate(vod_spend = dplyr::if_else(condition = name == 'decay',
  #                                               '.1,.7 ',
  #                                               '')) 
  # colors = NULL
  
  # checks  ####

  if (!is.logical(verbose)) {
    message("Warning: verbose provided must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  if(verbose){
    message("Running model...")
  }

  # check data is not provided
  if (is.null(data)) {
    message("- Error: No data provided for transformations. Returning NULL.")
    return(NULL)
  }
  if (!is.data.frame(data)) {
    message("- Error: data provided must be a data.frame. Returning NULL.")
    return(NULL)
  }

  # check dv is not provided
  if (is.null(dv)) {
    message("- Error: No dependent variable ('dv') provided. Returning NULL.")
    return(NULL)
  }

  # check pool_var
  if(is.null(pool_var)){

    if(verbose){
      message("- Warning: No pool_var provided. A new `total_pool` variable will be generated.")
    }

    pool_var = 'total_pool'
    data[pool_var] = pool_var
    pool_switch = FALSE

  }else{
    if(!(pool_var %in% colnames(data))){
      message("- Warning: pool variable not found in data. A new `total_pool` variable will be generated.")
      pool_var = 'total_pool'
      data[pool_var] = pool_var
      pool_switch = FALSE
    }else{
      pool_switch = TRUE
    }
  }
  
  # check normalise_by_pool
  if(is.null(normalise_by_pool)){
    normalise_by_pool = FALSE
  }

  # check id_var
  if(is.null(id_var)){
    if(verbose){
      message('- Warning: No id_var supplied. Generating a new `id_var` in data')
    }

    id_var = 'id'
    id_format = 'id'

    data = data %>%
      group_by(!!sym(pool_var)) %>%
      mutate(id = row_number()) %>%
      ungroup()
  } else if(!(id_var %in% colnames(data))){
    if(verbose){
      message('- Warning: id_var provided not found in data. Generating a new `id_var` in data')
    }

    id_var = 'id'
    id_format = 'id'
    
    data = data %>%
      group_by(!!sym(pool_var)) %>%
      mutate(id = row_number()) %>%
      ungroup()
  }
  if(is.null(id_format)){
    id_format = 'id'
  }
  
  # check trans_df
  if (is.null(trans_df)) {
    if (verbose) {
      message("- Warning: No trans_df provided. Setting default trans_df.")
    }
    trans_df = default_trans_df()
  } else{
    if (is.data.frame(trans_df)) {

      trans_df = check_trans_df(trans_df)

        if (verbose) {
          message("- Warning: missing colums were added to trans_df.")
        }
    } else{
      if (verbose) {
        message(
          '- Warning: trans_df must to of type data.frame. Setting to default trans_df (i.e. default_trans_df()).'
        )
      }
    }
  }

  # if model_table and ivs NOT provided
  if (is.null(model_table) & is.null(ivs)) {
    message(
      "- Error: No independent variable, 'ivs' nor 'model_table', provided. Returning NULL."
    )
    return(NULL)
  }
  # if model_table is provided
  if (!is.null(model_table)) {
    if (!is.null(ivs)) {
      if (verbose)
        message("- Info: will use variables from model_table and disregard 'ivs' argument.")
    }
    if (!is.data.frame(model_table)) {
      if (is.null(ivs)) {
        message("- Error: No 'ivs' nor 'model_table' provided. Returning NULL.")
        return(NULL)
      } else{
        message("- Warning: 'model_table' must be a data.frame. Using 'ivs' argument instead.")
      }
    } else{
      # use model table variables as ivs
      model_table = model_table %>%
        get_variable_t(excl_blanks = TRUE, trans_df = trans_df)

      ivs = model_table %>% filter(fixed=='') %>% pull(variable) %>% unique()
      ivs_t = model_table %>% filter(fixed=='') %>% pull(variable_t) %>% unique()
      fixed_ivs_t = model_table %>% filter(fixed!='')  %>% pull(variable_t) %>% unique()
    }
  } else{
    model_table = build_model_table(ivs = ivs, trans_df = trans_df)
    # use model table variables as ivs
    model_table = model_table %>%
      get_variable_t(excl_blanks = TRUE, trans_df = trans_df) %>% unique()

    ivs_t = model_table %>% pull(variable_t) %>% unique()
    fixed_ivs_t = NULL
  }


  # check categories in model_table
  if(!'category' %in% colnames(model_table)) {
    model_table$category = ""
  }
  
  # check categories in model_table
  if(is.null(colors)) {
    colors = linea::color_palette()
  }
  
  # check dark_mode
  if(is.null(dark_mode)){
    dark_mode = FALSE
  }
  
  

  # data    ####

  # generate norm_data
  if (normalise_by_pool) {
    norm_data = apply_normalisation(
      raw_data = data,
      pool_var = pool_var,
      dv = dv,
      verbose = verbose
    )
  } else{
    norm_data = data
  }

  # check norm_data for pool_mean
  if (length(norm_data) == 2) {
    pool_mean = norm_data$pool_mean
    norm_data = norm_data$data
  }

  trans_data = apply_transformation(
    raw_data = norm_data,
    trans_df = trans_df,
    model_table = model_table,
    pool_var = pool_var,
    verbose = verbose
  )

  # formula ####

  # build formula object
  formula = build_formula(dv = dv,
                          model_table = model_table,
                          trans_df = trans_df)
  
  # model   ####
  
  # run model

  model = lm(
    formula = formula,
    data = trans_data[, c(dv, ivs_t, fixed_ivs_t)]
  )

  # set colnames as ivs. bypass backticks added to ivs by lm()
  names(model$coefficients) = c("(Intercept)", ivs_t)
  colnames(model$qr$qr) = c("(Intercept)", ivs_t)

  # vif
  vif_df = car::vif(model) %>%
    TRY()
  
  # TODO: change to check number of vars
  # - vif useless with 1 var
  if(is.null(vif_df)){
    vif_df = c(vif='0')
  }
  
  vif_df = data.frame(vif = vif_df,
                      variable = ivs_t)
  
  if(exists('pool_mean')){
    model$pool_mean = pool_mean
  }
  
  model$id_var = id_var
  model$id_format = id_format
  model$tail_window = tail_window
  model$pool_var = pool_var
  model$dv = dv
  model$trans_df = trans_df
  model$model_table = model_table
  model$normalise_by_pool = normalise_by_pool
  model$pool_switch = pool_switch
  model$colors = colors
  model$dark_mode = dark_mode
  
  if(!is.null(categories)){
    model$categories = categories
  }
  
  output_model_table = model_table %>%
    filter(variable != "") %>%
    right_join(
      summary(model)$coefficients %>%
        data.frame() %>%
        rownames_to_column("variable_t") %>%
        rename(
          coef = 2,
          se = 3,
          t_stat = 4,
          p_value = 5
        ),
      by = "variable_t"
    ) %>%
    mutate(variable = if_else(variable_t == "(Intercept)",
                              variable_t,
                              variable)) %>%
    # replace na with 0
    zoo::na.fill('') %>%
    data.frame() %>%
    # tibble() %>%
    mutate(
      coef = as.numeric(coef),
      se = as.numeric(se),
      t_stat = as.numeric(t_stat),
      p_value = as.numeric(p_value),
      category = as.character(category)
    ) %>%
    zoo::na.fill("") %>%
    data.frame() %>%
    tibble() %>%
    mutate(
      coef = as.numeric(coef),
      se = as.numeric(se),
      t_stat = as.numeric(t_stat),
      p_value = as.numeric(p_value),
      category = as.character(category)
    ) %>%
    mutate(category = if_else(variable == "(Intercept)", "Base", category)) %>% 
    left_join(vif_df,by = c('variable_t'='variable')) #add vif

  # create moodel output table with tran's and stats's (e.g. tstats)
  model$output_model_table = output_model_table


  if(save_all_raw_data) {
    model$raw_data = data
  }else{
    model$raw_data = data[,c(id_var,pool_var,dv,ivs)]
  }

  # decomp  ####

  if (decompose) {
    decomp_list = decomping(
      tail_window = tail_window, 
      model = model,
      de_normalise = normalise_by_pool,
      categories = categories,
      verbose = verbose
    ) %>% TRY()

    if (is.null(decomp_list)) {
      if (verbose) {
        message('- Warning: decomposition failed. decomp_list will be NULL.')
      }
    } else{
      model$decomp_list = decomp_list
    }
  }

  # return model object
  return(model)
}


#' re_run_model
#'
#' Re-run a linear regression model
#'
#' Re-run a linear regression model using the function output of running \code{linea::run_model}.
#'
#' @export
#' @param model the model object used as the starting point of the re-run
#' @param data \code{data.frame} containing variables included in the model specification
#' @param dv string of the dependent variable name
#' @param ivs character vector of the independent variables names
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply
#' @param id_var string of id variable name (e.g. "date")
#' @param id_format string of id variable type (e.g. "weekly starting")
#' @param pool_var string of pool variable name (e.g. country)
#' @param model_table \code{data.frame} as created in the \code{build_model_table} function
#' @param verbose A boolean to specify whether to print warnings
#' @param normalise_by_pool A boolean to specify whether to apply the normalisation
#' @param decompose A boolean to specify whether to generate the model decomposition
#' @import tidyverse
#' @import tibble
#' @import zoo
#' @importFrom stats lm na.omit
#' @return Model object
#' @examples
#' model = run_model(mtcars,ivs = 'cyl', dv = 'mpg',save_all_raw_data = TRUE)
#' re_run_model(model,ivs = c('disp','cyl','wt'))
re_run_model = function(model,
                        data = NULL,
                        dv = NULL,
                        ivs = NULL,
                        trans_df = NULL,
                        id_var = NULL,
                        id_format = NULL,
                        pool_var = NULL,
                        model_table = NULL,
                        normalise_by_pool = NULL,
                        verbose = FALSE,
                        decompose = TRUE){

  # test    ----
  
  # model = pooled_model # from test file
  # data = NULL
  # dv = NULL
  # ivs = NULL
  # trans_df = NULL
  # id_var = NULL
  # pool_var = NULL
  # model_table = NULL
  # normalise_by_pool = NULL
  # verbose = TRUE
  # decompose = TRUE
   
  # checks  ####

  # check verbose
  if (!is.logical(verbose)) {
    message("Warning: verbose provided must be logical (TRUE or FALSE). Setting to FALSE.")
    verbose = FALSE
  }
  
  if(verbose){
    message('Re-running model...')
  }

  # check decompose
  if(is.null(decompose)){
    if(verbose)message("- Warning: decompose provided must be logical (TRUE or FALSE). Setting to TRUE.")
    decompose = TRUE
  }

  # check model
  if (!is(model,class2 = 'lm')) {
    message("- Error: model must be of type 'lm'. Returning NULL.")
    return(NULL)
  }

  # set defaults where needed
  if(is.null(data)){
    data = model$raw_data
  }
  if(is.null(dv)){
    dv = model$dv
  }

  if(is.null(ivs) & is.null(model_table)){
    model_table = model$model_table

    ivs = model$ivs
  }else if(!is.null(model_table)){
    if(!is.null(ivs)){
      if(verbose){
        message('- Warning: both model_table and ivs have been supplied. model_table will be used.')
      }
    }
    ivs = NULL
  }else if(!is.null(ivs)){
    model_table = NULL
  }

  if(is.null(trans_df)){
    trans_df = model$trans_df
  }
  if(is.null(pool_var)){
    pool_var = model$pool_var
  }
  if(is.null(id_var)){
    id_var = model$id_var
  }
  if(is.null(id_var_type)){
    id_var_type = model$id_var_type
  }
  if(is.null(normalise_by_pool)){
    normalise_by_pool = model$normalise_by_pool
  }

  
  # process ####
  model = run_model(data = data,
            dv = dv,
            ivs = ivs,
            trans_df = trans_df,
            id_var = id_var,
            id_format = id_format,
            model_table = model_table,
            pool_var = pool_var,
            normalise_by_pool = normalise_by_pool,
            verbose = verbose,
            decompose = decompose)
  
  
  if(verbose){
    message('Model has been re-run.')
  }
  
  return(model)
}
