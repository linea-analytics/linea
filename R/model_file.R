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
    'meta_data',
    'colors',
    'trans_df'
  )
  
  # check sheet names
  sheet_names = excel_sheets(model_file)
  
  sheet_check =  needed_sheets %in% sheet_names
  if (!all(sheet_check)) {
    cat("\nThese sheets were not found in model_file:\n- ")
    cat(paste0(needed_sheets[!sheet_check], collapse = "\n- "))
    cat("\nReturning NULL.\n")
    return(NULL)
  }
  
  print('model_file check passed')
  
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
    as.data.frame()
  categories = model_list$categories
  dv = model_list$dv$variable
  normalise_by_pool = model_list$normalise_by_pool$variable
  meta_data = model_list$meta_data
  id_var = model_list$id_var$variable
  data = model_list$data
  trans_df = model_list$trans_df
  
  model = run_model(
    data = data,
    dv = dv,
    model_table = model_table,
    trans_df = trans_df,
    meta_data = meta_data,
    id_var = id_var,
    verbose = verbose,
    normalise_by_pool = normalise_by_pool,
    categories = categories,
    save_raw_data = TRUE,
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
export_model = function(model,path = 'model.xlsx',overwrite = FALSE){
  model_list = list(
    data = model$data,
    model_table = model$model_table,
    dv = data.frame(variable = model$dv),
    categories = model$categories,
    id_var = data.frame(variable = model$id_var),
    id_format = data.frame(variable = model$id_var_format),
    normalise_by_pool = data.frame(variable = model$normalise_by_pool),
    meta_data = model$meta_data,
    colors = model$colors,
    trans_df = model$trans_df
  )
  
  write.xlsx(model_list, file = path, overwrite = overwrite)
  
}
