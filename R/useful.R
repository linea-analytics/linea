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
read_xcsv = function(file,
                     sheet = NULL,
                     verbose = FALSE) {
  if (!is.logical(verbose)) {
    message("Warning: verbose provided must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  # if the file ends with .csv read as csv
  if (endsWith(x = tolower(file) , suffix = ".csv")) {
    if (verbose)
      message('uploading csv')
    data = read_csv(file = file)
  }
  else if (endsWith(x = tolower(file) , suffix = ".xls") |
           endsWith(x = tolower(file) , suffix = ".xlsx") |
           endsWith(x = tolower(file) , suffix = ".xlsm")) {
    if (verbose)
      message('uploading excel')
    if (is.null(sheet)) {
      if (verbose)
        message('reading first sheet')
      data = read_excel(path = file)
    }
    if (!is.null(sheet)) {
      if (verbose)
        message(paste0('reading sheet ', sheet))
      data = read_excel(path = file, sheet = sheet)
    }
  }
  else{
    if (verbose)
      message("Error: file path must refer to csv or Excel (.xls, ,xlsm, .xlsx) file")
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
TRY = function(x, verbose = FALSE) {
  tryCatch(
    x,
    error = function(e) {
      if (verbose) {
        message("Warning: caught error below...")
        message(e)
      }
      return(NULL)
    }
  )
}


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
run_text = function(text, env) {
  eval(parse(text = text), envir = env)
}


#' check_currency
#'
#' check and convert to numeric
#'
#' Given a comma-separated string of symbols (e.g.  "$,£"), columns are converted to numeric.
#' Only columns containing the symbols are checked. 
#' Use `linea::check_numeric` for a more automated solution.
#'
#' @param text Code to run as string
#' @param currencies a comma-separated string of symbols
#' @export
#' @return parsed data.frame
check_currency = function(data, currencies = "$,£") {
  currencies = strsplit(currencies, split = ',')[[1]]
  
  for (curr in currencies) {
    data = data %>%
      mutate_if(~ any(str_detect(., paste0('^\\', curr)), na.rm = TRUE),
                ~ readr::parse_number(.))
  }
  
  return(data)
}

#' parsr
#'
#' an implementation of `readr::parse_number`
#'
#' an implementation of `readr::parse_number` which will look for numbers to parse, or alternatively guess using `readr::parse_guess`
#'
#' @param v value(s) to parse
#' @param guess a boolean to specify weather to guess data-type
#' @export
#' @return parsed value(s)
parsr = function(v, guess = TRUE) {
  res <- suppressWarnings(readr::parse_number(v))
  if (is.null(attr(res, "problems", exact = TRUE))) {
    res
  } else {
    if (guess) {
      readr::parse_guess(v)
    } else{
      v
    }
  }
}

#' check_numeric
#'
#' check_numeric using `parsr`
#'
#' An implementation of `readr::parse_number` which will look for numbers to parse, or alternatively guess using `readr::parse_guess`.
#' This process is applied to too all columns of the input data.frame `data`.
#'
#' @param data data.frame to check
#' @param guess a boolean to specify weather to guess data-type
#' @export
#' @return parsed data.frame
check_numeric = function(data, guess = TRUE) {
  data %>%
    mutate_all( ~ parsr(v = ., guess = guess))
}
