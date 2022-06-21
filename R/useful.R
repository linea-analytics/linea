#' read_xcsv
#' 
#' Reads flat files: either csv or excel
#' 
#' Reads csv or excel files with the suffixes csv, xls, xlsx, xlsm 
#' 
#' @param file The file path that points to either a csv or excel file ending in csv, xls, xlsx, or xlsm
#' @param sheet For excel files, the sheet name as a string or number as an integer
#' @param verbose A boolean to specify whether to print warnings
#' @return \code{data.frame} from flatfile
#' @export
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @examples 
#' read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv")  
read_xcsv = function(file, sheet = NULL, verbose = FALSE) {
  if(!is.logical(verbose)){
    message("Warning: verbose provided must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  # if the file ends with .csv read as csv
  if (endsWith(x = tolower(file) , suffix = ".csv")) {
    if(verbose)print('uploading csv')
    data = read_csv(file = file)
  }
  else if (endsWith(x = tolower(file) , suffix = ".xls") |
           endsWith(x = tolower(file) , suffix = ".xlsx") |
           endsWith(x = tolower(file) , suffix = ".xlsm")) {
    if(verbose)print('uploading excel')
    if(is.null(sheet)){
      if(verbose)print('reading first sheet')
      data = read_excel(path = file)
    }
    if(!is.null(sheet)){
      if(verbose)print(paste0('reading sheet ',sheet))
      data = read_excel(path = file,sheet=sheet)
    }
  }
  else{
    if(verbose)print("Error: file path must refer to csv or Excel (.xls, ,xlsm, .xlsx) file")
    return(NULL)
  }
  return(data)
}

#' TRY
#' 
#' TRY or NULL
#' 
#' A tryCatch implementation that returns NULL when an error is thrown 
#' 
#' @param x The expression to try
#' @param verbose boolean to specify whether to print the error
#' @export
#' @return expression output or \code{NULL} 
TRY = function(x,verbose = FALSE){
  tryCatch(
    x,
    error = function(e){
      if(verbose){
        message(e)
      }
      return(NULL)
    }
  )}


#' run_text
#' 
#' run text as R code
#' 
#' Run a text string as R code
#' 
#' @param text Code to run as string
#' @param env environment object specifying the environment
#' @export
#' @return text expression output
run_text = function(text,env){
  eval(parse(text = text),envir = env)
}
