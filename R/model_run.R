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
#' @param meta_data \code{data.frame} mapping variable names to their roles (i.e. POOL)
#' @param model_table \code{data.frame} as created in the \code{build_model_table} function
#' @param verbose A boolean to specify whether to print warnings
#' @param dv string specifying the dependent variable name
#' @return \code{list} containing a \code{tibble} of normalised data and a \code{tibble} of pool means
apply_normalisation = function(raw_data = NULL,
                               meta_data = NULL,
                               model_table = NULL,
                               dv = NULL,
                               verbose = FALSE){
  if(!is.logical(verbose)){
    cat("\n Warning: verbose provided must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }

  # if no raw_data is provided, end by returning NULL
  if (is.null(raw_data)) {
    if(verbose){
      cat("\n Error: No raw_data provided. Returning NULL.")
    }
    return(NULL)
  }
  # if raw_data is not a dataframe
  if(!is.data.frame(raw_data)){
    if(verbose){
      cat("\n Error: raw_data must be a data.frame. Returning NULL.")
    }
    return(NULL)
  }


  # if no meta_data is provided, end by returning raw_data
  if (is.null(meta_data)) {
    if(verbose){
      cat("\n Error: No meta_data provided. Returning original raw_data.")
    }
    return(raw_data)
  }
  # if meta_data is not a dataframe
  if(!is.data.frame(meta_data)){
    if(verbose){
      cat("\n Error: meta_data must be a data.frame. Returning original raw_data.")
    }
    return(raw_data)
  }


  # if the meta_data table doesnt contain a variables column, end by returning raw_data
  if(!all(c("variable","meta") %in% colnames(meta_data))){
    if(verbose){
      cat("\n Error: meta_data does not contain a 'variable' and a 'meta' columns. Returning original raw_data.")
    }
    return(raw_data)
  }
  # get all variables in meta_data
  variable = meta_data$variable

  # get data column names
  data_variables = colnames(raw_data)

  # get the pool variable
  pool_variable = TRY({
    variable[toupper(meta_data$meta) == "POOL"]
  })
  pool_mean = NULL

  # check if a pool variable is provided
  ## RETURNING RAW_DATA IF NO POOL_VARIABLE provided??
  if(is.null(pool_variable) | length(pool_variable) == 0) {
    if(verbose){
      cat("\n Warning: no POOL variable found in meta_data. A 'total_pool' variable was added.")
    }

    # if not provided create a unique one
    pool_variable = "total_pool"

    # add the pool variable to data
    raw_data = cbind(raw_data, total_pool = pool_variable)

    # get new data column names
    data_variables = colnames(raw_data)
  }

  # check if more than 1 pool variable is provided
  if (length(pool_variable) > 1) {
    if(verbose){
      cat("\n Error: More than 1 pool variable provided")
    }
    return(raw_data)
  }

  # check if pool variable in data variables
  if (!(pool_variable %in% data_variables)) {
    if(verbose){
      cat("\n Error: POOL variable not found in raw_data. Returning original raw_data.")
    }
    return(raw_data)
  }

  # check if ivs and dv are provided to select only relevant variables
  if(is.null(model_table)){
    if(verbose){
      cat("\n Warning: Normalising all raw_data as no model_table is provided.")
    }
  }else{
    if(!is.data.frame(model_table)){
      if(verbose){
        cat("\n Warning: model_table must be a data.frame. Normalising all raw_data.")
      }
    }else{
      if(!is.null(dv)){

        ivs = model_table$variable %>%
          unique()

        # check if ivs dv are in data
        if(all(c(ivs) %in% colnames(raw_data))){
          if(dv %in% colnames(raw_data)){

            # keep only relevant variables
            raw_data = raw_data[,c(pool_variable,dv,ivs)]

          }else{
            if(verbose){
              cat("\n Warning: dependent variable (dv) not found in raw_data. Normalising all raw_data.")
            }
          }
        }else{
          if(verbose){
            cat("\n Warning: variables from model_table not found in raw_data. Normalising all raw_data.")
          }
        }
      }else{
        if(verbose){
          cat("\n Warning: dependent variable (dv) not provided. Normalising all raw_data.")
        }
      }
    }
  }

  # in raw data, check for and drop NAs
  if(any(complete.cases(raw_data))) {
    if(verbose){
      cat("\n Warning: NA's found in raw_data will be dropped.")
    }
    raw_data = raw_data[complete.cases(raw_data), ]
  }


  if(dv %in% colnames(raw_data)){

    pool_mean = raw_data %>%
      group_by(!!sym(pool_variable)) %>%
      summarise(mean = mean(!!sym(dv))) %>%
      rename(pool = !!sym(pool_variable))

  }

  # remove the pool variable from the others
  variable = variable[variable != pool_variable]

  # for each variable
  for (var in variable) {
    # get that variable's transformation
    transformation = meta_data$meta[meta_data$variable == var] %>% toupper()

    # if STA apply pooled sta
    if (transformation == "STA") {
      raw_data = raw_data %>%
        group_by(!!sym(pool_variable)) %>%
        mutate(var = !!sym(var) / mean(!!sym(var), na.rm = TRUE)) %>%
        mutate(var = if_else(is.na(var), 0, var))

      # replace the raw variable with the STAed variable
      raw_data[, var] = raw_data[, "var"]
      raw_data[, "var"] = NULL

    }

  }
  return(list(data = raw_data,pool_mean = pool_mean))
}

#' default_trans_df
#'
#' The default transformation \code{data.frame}
#'
#' The default transformation \code{data.frame} containing the functions \code{decay}, \code{diminish}, \code{lag}, and \code{ma}
#'
#' @export
#' @return \code{data.frame} containing the transformations 
default_trans_df = function(){
  
  return(trans_df = data.frame(
    name = c('diminish', 'decay', 'lag', 'ma'),
    func = c(
      'linea::diminish(x,a)',
      'linea::decay(x,a)',
      'linea::lag(x,a)',
      'linea::ma(x,a)'
    ),
    order = 1:4
  ))
}


#' #' apply_transformation
#'
#' Transform data based on model table
#'
#' Transform data based on the model table by applying the transformation functions (e.g. \code{decay}, \code{diminish}, \code{lag}, and \code{ma}) with the specified parameters to the respective variable
#'
#' @export
#' @param data \code{data.frame} containing data for analysis
#' @param model_table \code{data.frame} as created in the \code{build_model_table} function
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply 
#' @param meta_data \code{data.frame} mapping variable names to their roles (i.e. POOL)
#' @param verbose A boolean to specify whether to print warnings
#' @return \code{tibble} of raw_data with added transformed variables
apply_transformation = function(data = NULL,
                                model_table = NULL,
                                trans_df = NULL,
                                meta_data = NULL,
                                verbose = FALSE) {
  # checks  #####
  
  # check verbose
  if(!is.logical(verbose)){
    cat("\n Warning: verbose provided must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  # check model table provided (not NULL)
  if(is.null(data)){
    if(verbose){
      cat("\n Error: No data provided for transformations. Returning NULL.")
    }
    return(NULL)
  }
  if(!is.data.frame(data)){
    if(verbose){
      cat("\n Error: data provided must be a data.frame. Returning NULL.")
    }
    return(NULL)
  }
  
  # check model table provided (not NULL)
  if(is.null(model_table)){
    if(verbose){
      cat("\n Warning: No model table provided for transformations. Returning raw data.")
    }
    return(data)
  }
  if(!is.data.frame(model_table)){
    if(verbose){
      cat("\n Warning: model table provided must be a data.frame. Returning raw data.")
    }
    return(data)
  }
  
  # check trans_df
  if(is.null(trans_df)){
    if(verbose){
      cat("\n Warning: no trans_df provided. Setting default trans_df.")
    }
    trans_df = data.frame(
      name = c('diminish','decay','lag','ma'),
      func = c('linea::diminish(x,a)','linea::decay(x,a)','linea::lag(x,a)','linea::ma(x,a)'),
      order = 1:4
    )
  }
  if(!is.data.frame(model_table)){
    if(verbose){
      cat("\n Warning: trans_df provided must be a data.frame. Setting default trans_df.")
    }
    trans_df = data.frame(
      name = c('diminish','decay','lag','ma'),
      func = c('linea::diminish(x,a)','linea::decay(x,a)','linea::lag(x,a)','linea::ma(x,a)'),
      order = 1:4
    )
  }
  
  # get variables from model table
  model_table = model_table %>%
    get_variable_t()
  
  variable_t = model_table$variable_t
  variable = model_table$variable
  trans = trans_df$name
  
  # if a meta table is provided try to extract the POOL variable
  if(is.null(meta_data)| !is.data.frame(meta_data)){
    # else create a pool variable "total"...
    if(verbose){
      if(is.null(meta_data)){
        cat("\n Info: no meta_data provided. No groups (i.e. POOLs) used in transformations.")
      }
      if(!is.null(meta_data) & !is.data.frame(meta_data)){
        cat("\n Warning: meta_data provided must be a data.frame. No groups (i.e. POOLs) used in transformations.")
      }
    }
    pool = "total_pool"
    # ...and add it to the data
    
    # if default pool variable already in use replace it
    if(pool %in% colnames(data)){
      if(verbose){
        cat("\n Info: 'total_pool' variable will be added/replaced from data.")
      }
      
      data$total_pool = NULL
      
    }
    
    data = tibble(data, total_pool = pool)
    
  } else if (!is.null(meta_data)) {
    pool = TRY({
      meta_data %>%
        filter(meta == "POOL") %>%
        pull(variable)
    })
    
    # if there is a pool variable in the meta data...
    # ...but has zero
    if (!is.null(pool)) {
      if (length(pool) == 0) {
        if(verbose)print("Warning: no pool variable found in meta_data")
        pool = "total_pool"
        
        # if default pool variable already in use replace it
        if(pool %in% colnames(data)){
          if(verbose)print("Warning: 'total_pool' variable will be added/replaced from data")
          
          data$total_pool = NULL
          
        }
        
        data = tibble(data, total_pool = pool)
      }
    } else{
      # else create a pool variable "total"...
      if(verbose)print("Warning: no pool variable found in meta_data")
      
      # if default pool variable already in use replace it
      if("total_pool" %in% colnames(data)){
        if(verbose)print("Warning: 'total_pool' variable will be added/replaced from data")
        
        data$total_pool = NULL
        
      }
      
      pool = "total_pool"
      # ...and add it to the data
      data = tibble(data, total_pool = pool)
      
    }
  }
  
  # process ####
  
  # for each variable...
  for(i in 1:length(variable)){
    
    var_t_name = variable_t[i]
    var_name = variable[i]
    
    # skip if blank
    if(var_name == ""){
      next
    }
    
    # data[,var_t_name] = data[,var_name]
    data[,'temp_var'] = data[,var_name]
    
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
      
      groups = data %>%
        pull(!!sym(pool)) %>%
        unique()
      
      for(g in groups){
        
        x = data$temp_var[data[,pool]==g]
        x = t_func %>% run_text(env = e)
        data$temp_var[data[,pool]==g] = x
        
      }
    }
    
    data[,var_t_name] = data[,'temp_var']
    data[,'temp_var'] = NULL
  }
  
  return(data)
  
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
vapply_transformation = function(v,trans_df = NULL,verbose = FALSE){
  # checks  ####
  
  # check verbose
  if (!is.logical(verbose)) {
    cat("\n Warning: verbose provided must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  # check trans_df
  if (is.null(trans_df)) {
    if (verbose) {
      cat("\n Warning: no trans_df provided. Setting default trans_df.")
    }
    trans_df = data.frame(
      name = c('diminish', 'decay', 'lag', 'ma'),
      func = c(
        'linea::diminish(x,a)',
        'linea::decay(x,a)',
        'linea::lag(x,a)',
        'linea::ma(x,a)'
      ),
      order = 1:4,
      params = rep('', 4)
    )
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
#' Run a linear regression model that captures the transformations applied in the \code{model_table} and the normalisation described in the \code{meta_data}.
#' A model can be run also by only supplying a dependent variable name \code{dv}, a vector of independent variable names dependent variable \code{ivs}, and the data that contains these.
#'
#' @export
#' @param data \code{data.frame} containing variables included in the model specification
#' @param dv string of the dependent variable name
#' @param ivs character vector of the independent variables names
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply 
#' @param meta_data \code{data.frame} mapping variable names to their roles (i.e. POOL)
#' @param id_var string of id variable name (e.g. date)
#' @param model_table \code{data.frame} as created in the \code{build_model_table} function
#' @param verbose A boolean to specify whether to print warnings
#' @param normalise_by_pool A boolean to specify whether to apply the normalisation
#' @param save_raw_data A boolean to specify whether to save all input data variables to the model object 
#' @param decompose A boolean to specify whether to generate the model decomposition
#' @param categories \code{data.frame} mapping variables to groups
#' @import tidyverse
#' @import tibble
#' @import zoo
#' @importFrom stats lm na.omit
#' @return Model object
#' @examples
#' run_model(data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv"),
#'           dv = 'ecommerce',
#'           ivs = c('christmas','black.friday'))
#' run_model(data = mtcars,dv = 'mpg',ivs = c('disp','cyl'))
run_model = function(data = NULL,
                     dv = NULL,
                     ivs = NULL,
                     trans_df = NULL,
                     meta_data = NULL,
                     id_var = NULL,
                     model_table = NULL,
                     verbose = FALSE,
                     normalise_by_pool = FALSE,
                     save_raw_data = TRUE,
                     decompose = TRUE,
                     categories = NULL) {
  # checks  ####
  
  if (!is.logical(verbose)) {
    cat(
      "\n Warning: verbose provided must be logical (TRUE or FALSE). Setting to False."
    )
    verbose = FALSE
  }
  
  # check data is not provided
  if (is.null(data)) {
    if (verbose) {
      cat("\n Error: No data provided for transformations. Returning NULL.")
    }
    return(NULL)
  }
  if (!is.data.frame(data)) {
    if (verbose) {
      cat("\n Error: data provided must be a data.frame. Returning NULL.")
    }
    return(NULL)
  }
  
  # check dv is not provided
  if (is.null(dv)) {
    cat("\n Error: no dependent variable ('dv') provided. Returning NULL.")
    return(NULL)
  }
  
  # check meta_data
  if (is.null(meta_data)) {
    if (verbose) {
      cat("\n Info: No meta_data provided for transformations and normalisation.")
    }
  }
  else if (!is.data.frame(meta_data)) {
    if (verbose) {
      cat(
        "\n Warning: meta_data provided must be a data.frame. Not using 'meta_data' provided for transformations and normalisation."
      )
    }
  }else if(nrow(meta_data)==0){
    if (verbose) {
      cat(
        "\n Warning: meta_data provided has zero rows. Not using 'meta_data' provided for transformations and normalisation."
      )
    }
  }
  
  # if model_table and ivs not provided
  if (is.null(model_table) & is.null(ivs)) {
    if (verbose) {
      cat(
        "\n Error: no independent variable, 'ivs' nor 'model_table', provided. Returning NULL."
      )
    }
    return(NULL)
  }
  # if model_table is provided
  if(!is.null(model_table)) {
    if (!is.null(ivs)) {
      if (verbose)
        cat("\n Info: will use variables from model_table and disregard 'ivs' argument.")
    }
    if (!is.data.frame(model_table)) {
      if (is.null(ivs)) {
        cat("\n Error: no 'ivs' nor 'model_table' provided. Returning NULL.")
        return(NULL)
      } else{
        cat("\n Warning: 'model_table' must be a data.frame. Using 'ivs' argument instead.")
      }
    } else{
      # use model table variables as ivs
      model_table = model_table %>%
        get_variable_t(excl_blanks = TRUE)
      
      ivs = model_table %>% pull(variable) %>% unique()
      ivs_t = model_table %>% pull(variable_t) %>% unique()
    }
  }else{
    model_table = build_model_table(ivs = ivs)
    # use model table variables as ivs
    model_table = model_table %>%
      get_variable_t(excl_blanks = TRUE) %>% unique()
    
    ivs_t = model_table %>% pull(variable_t) %>% unique()
  }
  
  # check trans_df
  if(is.null(trans_df)){
    if(verbose){
      cat("\n Warning: no trans_df provided. Setting default trans_df.")
    }
    trans_df = data.frame(
      name = c('diminish','decay','lag','ma'),
      func = c('linea::diminish(x,a)','linea::decay(x,a)','linea::lag(x,a)','linea::ma(x,a)'),
      order = 1:4
    )
  }
  
  
  # check categories in model_table
  if(!('category' %in% colnames(model_table))){
    model_table$category = ""
  }
  
  # formula ####
  
  # build formula object
  formula = build_formula(dv = dv, ivs = ivs_t)
  
  
  # data    ####
  
  # generate norm_data
  if(normalise_by_pool){
    norm_data = apply_normalisation(
      raw_data = data,
      model_table =  model_table,
      meta_data = meta_data,
      dv = dv,
      verbose = verbose
    )
  }else{
    norm_data = data
  }
  
  # check norm_data
  if (length(norm_data) == 2) {
    pool_mean = norm_data$pool_mean
    norm_data = norm_data$data
  }
  
  trans_data = apply_transformation(
    data = norm_data,
    trans_df = trans_df,
    model_table = model_table,
    meta_data = meta_data,
    verbose = verbose
  )
  
  # model   ####
  
  # run model on norm_data
  model = lm(formula = formula, data = trans_data[,c(dv,ivs_t)])
  
  # add meta_data to mdoel object
  model$meta_data = meta_data
  
  # set colnames as ivs. bypass backticks added to ivs by lm()
  names(model$coefficients) = c("(Intercept)", ivs_t)
  colnames(model$qr$qr) = c("(Intercept)", ivs_t)
  #names(model$effects)[1:(length(ivs_t))] = c("(Intercept)", ivs)
  
  # add dv, trans_df, and model_table to mdoel object
  model$dv = dv # add dv and model_table to mdoel object
  model$trans_df = trans_df
  model$model_table = model_table
  
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
    na.fill('') %>%
    data.frame() %>%
    # tibble() %>%
    mutate(
      coef = as.numeric(coef),
      se = as.numeric(se),
      t_stat = as.numeric(t_stat),
      p_value = as.numeric(p_value),
      category = as.character(category)
    ) %>%
    na.fill("") %>%
    data.frame() %>%
    tibble() %>%
    mutate(
      coef = as.numeric(coef),
      se = as.numeric(se),
      t_stat = as.numeric(t_stat),
      p_value = as.numeric(p_value),
      category = as.character(category)
    ) %>%
    mutate(
      category = if_else(variable == "(Intercept)","Base",category)
    )
  
  # create moodel output table with tran's and stats's (e.g. tstats)
  model$output_model_table = output_model_table
  
  
  if (exists("pool_mean")) {
    model$pool_mean = pool_mean
  }
  
  if(exists("id_var")){
    model$id_var = id_var
  }
  
  if(save_raw_data){
    model$data = data
  }
  
  model$normalise_by_pool = normalise_by_pool 
  
  # decomp  ####
  if (decompose) {
    decomp_list = decomping(
      model = model,
      raw_data = data,
      de_normalise = normalise_by_pool,
      categories = categories,
      verbose = verbose,
      id_var = id_var
    ) %>% TRY()
    
    if(is.null(decomp_list)){
      if(verbose){
        cat('Warning: decomposition failed. decomp_list will be NULL.\n')
      }
    }else{
      model$decomp_list = decomp_list
    }
  }
  
  # return model object
  return(model)
}
