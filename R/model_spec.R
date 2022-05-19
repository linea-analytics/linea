#' default_trans_df
#'
#' The default trans_df
#'
#' Generate the default trans_df \code{data.frame} with functions from linea:
#' - decay
#' - diminish
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
      name = c('diminish','hill','decay','lag','ma'),
      func = c('linea::diminish(x,a)','linea::hill_function(x,a,b,c)','linea::decay(x,a)','linea::lag(x,a)','linea::ma(x,a)'),
      order = 1:5
    )
  }

  else{
    trans_df = data.frame(
      name = c('diminish','hill'),
      func = c('linea::diminish(x,a)','linea::hill_function(x,a,b)'),
      order = 1:2
    )
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
#' build_model_table(colnames(mtcars)) %>%
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
#' @import dplyr
#' @importFrom stats formula
#' @return a \code{formula} object
build_formula = function(dv, ivs, model_table = NULL) {

  if(!is.null(model_table)){

    model_table = get_variable_t(model_table)

    ivs = model_table %>%
      pull(variable_t)

  }
  else{
    ivs = ivs[(!is.null(ivs))&(!is.na(ivs))&(ivs != "")]
  }
  f = formula(paste0("`",
                     dv,
                     "` ~ `",
                     paste0(ivs,
                            collapse = "` + `"),
                     "`"))
}
