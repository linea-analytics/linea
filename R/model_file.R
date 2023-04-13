#' check_model_file
#'
#' Check the excel model file
#'
#' Check the model file contains all the needed sheets.
#'
#' @param model_file File path to the model file as string
#' @param verbose Boolean to specify whether to return the checked file
#' @param return_list Boolean to specify whether to print warnings
#' @import readxl
#' @return Messages and the checked file
#' @export
check_model_file = function(model_file,verbose = FALSE,return_list = TRUE){
  # checks   ####
  if (!is.logical(verbose)) {
    print("verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  if (!is.character(model_file)) {
    if (verbose)
      print('model_file must be a character string file path. Returning NULL.')
    return(NULL)
  }
  if (!any(endsWith(model_file, suffix = c(".xlsx", ".xls")))) {
    if (verbose)
      print("model_file must point to an Excel file (i.e. .xlsx or .xls). Returning NULL.")
    return(NULL)
  }

  # process  ####

  # model file sheets
  needed_sheets = c(
    'data',
    'model_table',
    'dv',
    'categories',
    'id_var',
    'id_format',
    'normalise_by_pool',
    'pool_var',
    'colors',
    'trans_df'
  )

  # check sheet names
  sheet_names = excel_sheets(model_file)

  sheet_check =  needed_sheets %in% sheet_names
  if (!all(sheet_check)) {
    message("These sheets were not found in model_file:- ")
    message(paste0(needed_sheets[!sheet_check], collapse = "- "))
    message("Returning NULL.")
    return(NULL)
  }

  if(verbose){
    message('Info: check_model_file passed.')
  }

  if (return_list) {
    # import excel file sheets to list
    model_file_list = lapply(sheet_names, function(s) {
      read_xlsx(path = model_file,
                sheet = s)
    })

    # use excel sheet names as list names
    names(model_file_list) = sheet_names

    return(model_file_list)
  }
}

#' import_model
#'
#' Import and run the excel model file
#'
#' Import and run the excel model file using \code{check_model_file} and \code{run_model} functions
#'
#' @param path File path to the model file as string
#' @param verbose Boolean to specify whether to return the checked file
#' @import dplyr
#' @import zoo
#' @return model object
#' @export
import_model = function(path, verbose = FALSE){
  # import checked filed
  model_list = check_model_file(model_file = path, verbose = verbose)
  if (is.null(model_list)) {
    return(NULL)
  }

  # extract model settings (data,dv,ivs,etc...)
  model_table = model_list$model_table %>%
    zoo::na.fill('') %>%
    as.data.frame() %>% 
    select(-coef,-t_stat,-vif,-p_value,-se)
  categories = model_list$categories
  dv = model_list$dv$variable
  normalise_by_pool = model_list$normalise_by_pool$variable
  pool_var = model_list$pool_var$variable
  id_var = model_list$id_var$variable
  data = model_list$data
  trans_df = model_list$trans_df

  model = run_model(
    data = data,
    dv = dv,
    model_table = model_table,
    trans_df = trans_df,
    pool_var = pool_var,
    id_var = id_var,
    verbose = verbose,
    normalise_by_pool = normalise_by_pool,
    categories = categories,
    save_all_raw_data = TRUE,
    decompose = TRUE
  )

  return(model)
}


#' export_model
#'
#' Export model to excel file
#'
#' Export a model to an excel file
#'
#' @param model Model object
#' @param path File path to the model file as string
#' @param overwrite Boolean to specify whether to overwrite the file in the specified path
#' @importFrom openxlsx write.xlsx
#' @export
export_model = function(model,path = NULL,overwrite = FALSE,decomp = TRUE){
  
  if(is.null(path)){
    path = paste0(model$dv,"_",Sys.Date(),".xlsx")
  }
  
  if(decomp){
    # check if model contains decomp
    if("decomp_list" %in% names(model)){
      
      model_list = list(
        data = model$raw_data,
        model_table = model$output_model_table,
        dv = data.frame(variable = model$dv),
        categories = model$categories,
        variable_decomposition = model$decomp_list$variable_decomp,
        category_decomposition = model$decomp_list$category_decomp,
        fitted_values = model$decomp_list$fitted_values,
        id_var = data.frame(variable = model$id_var),
        id_format = data.frame(variable = model$id_format),
        id_date_format = data.frame(variable = model$id_date_format),
        normalise_by_pool = data.frame(variable = model$normalise_by_pool),
        pool_var = data.frame(variable = model$pool_var),
        pool_switch = data.frame(variable = model$pool_switch),
        colors = model$colors,
        trans_df = model$trans_df
      )
    }else{
      message("Warning: `decomp` input set to TRUE but no `decomp_list` found in `model`. 
              Exporting model without decomposition.")
      
      model_list = list(
        data = model$raw_data,
        model_table = model$output_model_table,
        dv = data.frame(variable = model$dv),
        categories = model$categories,
        id_var = data.frame(variable = model$id_var),
        id_format = data.frame(variable = model$id_format),
        id_date_format = data.frame(variable = model$id_date_format),
        normalise_by_pool = data.frame(variable = model$normalise_by_pool),
        pool_var = data.frame(variable = model$pool_var),
        pool_switch = data.frame(variable = model$pool_switch),
        colors = model$colors,
        trans_df = model$trans_df
      )
    }
  }
  

  openxlsx::write.xlsx(model_list, file = path, overwrite = overwrite)

}
