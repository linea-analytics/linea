#' default_trans_df
#'
#' The default trans_df
#'
#' Generate the default trans_df \code{data.frame} with functions from linea:
#' - decay
#' - hill_function
#' - ma
#' - lag
#'
#' @export
#' @param ts boolean to specify if time-series or not
#' @return \code{data.frmae} of trans_df
#' @examples
#' default_trans_df()
#' default_trans_df(ts = TRUE)
default_trans_df = function(ts = TRUE){

  if(ts){
    trans_df =  data.frame(
      name = c('hill','decay','lag','ma'),
      ts = c(FALSE,TRUE,TRUE,TRUE),
      func = c('linea::hill_function(x,a,b)','linea::decay(x,a)','linea::lag(x,a)','linea::ma(x,a)'),
      order = 1:4
    )
  }

  else{
    trans_df = data.frame(
      name = c('diminish','hill'),
      ts = c(FALSE,FALSE),
      func = c('linea::diminish(x,a)','linea::hill_function(x,a,b)'),
      order = 1:2
    )
  }

  return(trans_df)
}


#' check_trans_df
#'
#' Check trans_df based on default_trans_df
#'
#' Check that the trans_df \code{data.frame} contains all the necessary columns.
#' If not, create them and return the amended trans_df.
#'
#' @export
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply
#' @return \code{data.frmae} of trans_df
#' @examples
#' default_trans_df() %>% check_trans_df()
check_trans_df = function(trans_df){

  default_cols = colnames(default_trans_df())
  # if some cols missing
  if(!all(default_cols%in%colnames(trans_df))){
    # generate missing cols
    missing_cols = default_cols[!(default_cols%in%colnames(trans_df))]
    for(i in missing_cols){
      if(i == 'name'){
        # specific basic name
        trans_df[,i] = 'custom_linea_trans_0'
      }
      if(i == 'ts'){
        # not time series
        trans_df[,i] = FALSE
      }
      if(i == 'func'){
        # simple polynomial
        trans_df[,i] = 'x^a'
      }
      if(i == 'order'){
        # last in order
        trans_df[,i] = nrow(trans_df)
      }
    }
  }
  return(trans_df)
}

#' build_model_table
#'
#' Build an empty model table
#'
#' Build an empty table as a template to capture model predictors, and transformation parameters
#'
#' @export
#' @import tibble
#' @import dplyr
#' @param ivs character vector of variables
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply
#' @param ts boolean to specify if time-series or not
#' @return \code{tibble} of model table
#' @examples
#' build_model_table(c('x1','x2'))
#' build_model_table(colnames(mtcars))
build_model_table = function(ivs,trans_df = NULL,ts = TRUE){

  if(is.null(trans_df)){
    trans_df = default_trans_df(ts = ts)
  }

  # rows and cols
  rows = length(ivs)
  cols = c(trans_df %>%
             arrange(order) %>%
             pull(name))

  # build model table
  model_table = data.frame(matrix(ncol = length(cols), nrow = rows,data = ''))
  colnames(model_table) = cols
  model_table$variable = ivs
  model_table$fixed = ''
  model_table$category = ''

  return(model_table)

}

#' check_model_table
#'
#' Check `model_table`
#'
#' Check the columns of a `model_table` and generate any missing ones where possible.
#'
#' @export
#' @import tibble
#' @import dplyr
#' @param model_table \code{tibble}/ \code{data.frame} as created in the \code{build_model_table} function
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply
#' @param verbose A boolean to specify whether to print warnings
#' @return \code{tibble} of model table
check_model_table = function(model_table,verbose = FALSE,trans_df = NULL){
  
  if(!is.logical(verbose)){
    message("Warning: `verbose` must be logical. Seeting to TRUE.")
    verbose = TRUE
  }
  
  if(!is.data.frame(model_table)){
    message("Error: `mdoel_table` must be a dataframe. returning NULL.")
    return(NULL)
  }
  
  if(is.null(trans_df)){
    if(verbose){
      message("Warning: no trans_df provided. Setting to `default_trans_df`.")
      trans_df = default_trans_df()
    }
  }
  
  # ALTER FOR BAYESIAN
  columns = c(trans_df$name,"fixed","category","variable","variable_t")
  
  if(!all(columns %in% colnames(model_table))){
    
    missing_cols = columns[!columns %in% colnames(model_table)]
    if(verbose)message("Warning: column(s) missing from `model_table`:",paste0(missing_cols,collapse = ", "))
    
    if(!"variable" %in% colnames(model_table)){
      message("Error: `variable` column must be present in `model_table`. Returning NULL.")
      return(NULL)
    }
    
    for(col in c("fixed","category",trans_df$name)){
      if(!col %in% colnames(model_table)){
        if(verbose)message("- Adding blank `",col,"` column to `model_table`.")
        model_table[col] = ""
      }
    }
    
    if(!"variable_t" %in% colnames(model_table)){
      if(verbose)message("- Generating `variable_t`.")
      model_table = model_table %>% 
        get_variable_t(trans_df = trans_df)
    }
    
  }
  
  return(model_table)
  
}


#' get_variable_t
#'
#' Generate more specific variable names
#'
#' Generate variable names that capture the transformations applied
#'
#' @export
#' @param model_table \code{tibble}/ \code{data.frame} as created in the \code{build_model_table} function
#' @param excl_intercept Boolean to specify whether to drop the "(Intercept)" row of the model table
#' @param excl_dup Boolean to specify whether to drop the duplicated rows of the model table
#' @param excl_blanks Boolean to specify whether to drop the blank rows of the model
#' @param trans_df \code{data.frame} defining the non-linear transformations
#' @import dplyr
#' @return \code{tibble} of model table with added variable_t column
#' @examples
#' model_table = build_model_table(colnames(mtcars)) %>%
#'    get_variable_t()
get_variable_t = function(model_table,
                          excl_intercept = TRUE,
                          excl_dup = TRUE,
                          excl_blanks = FALSE,
                          trans_df = NULL) {

  if(is.null(trans_df)){
    trans_df = default_trans_df()
  }

  if (excl_intercept) {
    model_table = model_table %>%
      filter(variable != "(Intercept)")
  }
  
  if (excl_blanks) {
    model_table = model_table %>%
      filter(variable != "") %>%
      filter(!is.na(variable)) %>%
      filter(!is.null(variable))

  }
  if (excl_dup) {
    model_table = model_table[!duplicated(model_table) | model_table$variable=="",]
  }

  model_table = model_table %>%
    mutate(variable_t = variable)

  trans = trans_df$name

  for(t in trans){
    model_table = model_table %>%
      mutate(variable_t = if_else(!!sym(t) != 0 & !!sym(t) != "",
                                  paste0(variable_t, "_",t,"_", !!sym(t)),
                                  variable_t))
  }

  return(model_table)

}

#' build_formula
#'
#' Build a formula (e.g. y ~ x1 + x2)
#'
#' Build a formula (e.g. y ~ x1 + x2) based on a dependent variable name and a model table or independent variables' names' vector
#'
#' @export
#' @param dv string of dependent variable name
#' @param ivs character vector of independent variable names
#' @param model_table \code{tibble}/ \code{data.frame} as created in the \code{build_model_table} function
#' @param trans_df \code{data.frame} defining the non-linear transformations to apply
#' @import dplyr
#' @importFrom stats formula
#' @return a \code{formula} object
build_formula = function(dv, ivs, model_table = NULL, trans_df = NULL) {

  if(!is.null(model_table)){
    
    if(is.null(trans_df)){
      trans_df = default_trans_df()
    }
    
    model_table = get_variable_t(model_table,trans_df = trans_df)
    
    ivs = model_table %>%
      filter(fixed == '') %>% # remove variables with fixed coefficients
      pull(variable_t) # extract variable names
    
    fixed_ivs = model_table %>%
      filter(fixed != '') %>% # remove variables with fixed coefficients
      pull(variable_t) # extract variable names
    
    if(length(fixed_ivs)==0){
      f = formula(paste0("`",
                         dv,
                         "` ~ `",
                         paste0(ivs,
                                collapse = "` + `"),
                         "`"))
      return(f)
    }
    
    fixed_coef = model_table %>%
      filter(fixed != '') %>% # remove variables with fixed coefficients
      pull(fixed) # extract variable names
    
    if(length(ivs)==0){
      f = formula(paste0("`",
                         dv,
                         "` ~ ",
                         paste0(
                           'offset(', paste0(fixed_coef, " * `", fixed_ivs), '`)', collapse = ' + '
                         )))
      return(f)
    }
    
    f = formula(paste0(
      "`",
      dv,
      "` ~ `",
      paste0(ivs,
             collapse = "` + `"),
      "`  + ",
      paste0('offset(', paste0(fixed_coef, " * `", fixed_ivs), '`)', collapse = ' + ')
    ))
  }else{
    ivs = ivs[(!is.null(ivs))&(!is.na(ivs))&(ivs != "")]
    
    
    f = formula(paste0("`",
                       dv,
                       "` ~ `",
                       paste0(ivs,
                              collapse = "` + `"),
                       "`"))
  }
  return(f)
}
