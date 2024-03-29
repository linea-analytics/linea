#' gt_f
#' 
#' Google Trends function
#' 
#' Google Trends data based on input data, for dates, and keyword
#' 
#' @export
#' @import tidyverse
#' @import tibble
#' @import gtrendsR
#' @param data \code{data.frame} containing data for analysis
#' @param kw a string of the search keyword
#' @param date_col a string specifying the date column name
#' @param date_type The date column type as either of the following strings:'weekly starting','weekly ending','daily'
#' @param geo a string specifying the country code of the search found in \code{countrycode::codelist}
#' @param verbose A boolean to specify whether to print warnings
#' @param graceful A boolean to specify whether to fail gracefully 
#' @param append a boolean specifying whether to return the original data.frame as well as the added column
#' @return \code{data.frame} of the original data with the added google trend column
#' @examples 
#' data = linea::sales_ts %>% 
#'   gt_f(kw = 'covid',date_col = 'week') %>% 
#'   gt_f(kw = 'bitcoin',date_col = 'week')
#' 
gt_f = function(data,
                kw,
                date_col = NULL,
                date_type = NULL,
                geo = "all",
                verbose = FALSE,
                graceful = TRUE,
                append = TRUE) {
  # test                ####
  
  # data = linea::sales_ts
  # kw = 'bitcoin'
  # date_col = "week"
  # date_type = "weekly starting"
  # geo = "all"
  # append = TRUE
  # graceful = TRUE
  # verbose = TRUE
  
  # date_col = "date"
  # kw = 'bitcoin'
  # data = linea::cran_downloads
  # date_type = "daily"
  # geo = "all"
  # append = TRUE
  # verbose = TRUE
  # graceful = TRUE
  
  # data = data.frame(weeks = seq(Sys.Date() - 750, Sys.Date(), by = 7))
  # kw = 'amazon'
  # date_col = 'weeks'
  # date_type = 'weekly starting'
  # geo = "all"
  # verbose = TRUE
  # append = TRUE
  # graceful = TRUE

  
  # checks              ####
  
  if(!is.logical(verbose)){
    message("verbose must be logical (TRUE or FALSE). Setting to TRUE.")
    verbose = TRUE
  }
  if(verbose){
    message("Gathering Google-Trends data...")
  }
  if(!is.data.frame(data)){
    message('- Error: data must be a dataframe. Returning NULL.')
    return(NULL)
  }
  if(is.null(date_col)){
    message('- Error: date_col must be provided. Returning NULL.')
    return(NULL)
  }
  if(is.null(date_type)){
    message('- Error: date_type must be provided. Returning NULL.')
    return(NULL)
  }
  
  # check data provided contains date_col
  if(!(date_col %in% colnames(data))){
    message('- Error: data must contain date_col_name. Returning NULL.')
    return(NULL)
  }
  
  # process             ####
  
  # get dates from data
  dates = data %>% 
    pull(sym(date_col)) %>% 
    as.Date() %>% 
    unique()
  
  min_date = min(dates) - 7
  max_date = max(dates)
  
  if(max_date > Sys.Date()){
    if(verbose){
      message("Warning: max_date set to today as google trends are not forecasts.")
      }
    max_date = Sys.Date()
  }
  
  # create time string for gtrends request
  time_str = paste0(min_date, " ", max_date)
  
  # make gtrends request
  if(geo=="all"){
    gt = gtrendsR::gtrends(
      keyword = kw,
      time = time_str,
      onlyInterest = TRUE
    ) %>% TRY(verbose = verbose)
  }else{
    gt = gtrendsR::gtrends(
      keyword = kw,
      geo = geo,
      time = time_str,
      onlyInterest = TRUE
    ) %>% TRY(verbose = verbose)
  }
  
  # check gtrends call
  if(is.null(gt)){
    if(graceful){
      message('Error: gtrends function failed. Check internet connection or attempt installing the gtrends package separately. Returning data.')
      return(data)
    }else{
      message('Error: gtrends function failed. Check internet connection or attempt installing the gtrends package separately. Returning NULL.')
      return(NULL) 
    }
  }
  
  gt = gt[[1]]%>%
    select(date, hits) %>%
    tibble()
  
  # save gtrends dates vector
  gt_dates = gt$date
  
  # get gtrends date type
  if(is_daily(gt$date))gt_date_type = "daily"
  if(is_weekly(gt$date))gt_date_type = "weekly starting"
  
  
  # create daily dates depending on date_type
  extremes = gt_dates %>% 
    as.Date() %>% 
    first_last_dates(date_type = gt_date_type)
  gt_first_day = extremes[[1]]
  gt_last_day = extremes[[2]]
  
  gt_daily_map = tibble(
    week = rep(gt_dates,each=7),
    day = seq(gt_first_day,gt_last_day,by = "d")
  )
  
  # get daily gt
  gt_daily = gt_daily_map %>%
    left_join(gt,by=c("week"="date")) %>%
    select(-week)
  
  if(is.character(gt_daily$hits)){
    
    gt_daily = gt_daily %>%
      mutate(hits = if_else(hits == "<1", "0", hits)) %>%
      mutate(hits = as.numeric(hits))
    
  }
  
  # create gt df for join depending on data date_type
  if(date_type == "daily"){
    
    df = gt_daily
    
  } else if(date_type == "weekly starting" | date_type == "weekly ending"){
    
    extremes = dates %>% 
      as.Date() %>% 
      first_last_dates(date_type = date_type)
    first_day = extremes[[1]]
    last_day = extremes[[2]]
    
    daily_map = tibble(
      week = rep(dates,each=7),
      day = seq(first_day,last_day,by = "d")
    )
    
    # get gtrends with correct week
    df = daily_map %>%
      left_join(gt_daily,by = "day")%>%
      group_by(week) %>%
      summarise(hits = mean(hits,na.rm = TRUE)) %>%
      ungroup()
    
  }
  
  colnames(df)[1] = date_col
  colnames(df)[2] = paste0(c(kw,geo,'gt'),collapse = '_')
  
  if(append){
    df = data %>% 
      left_join(df,by=date_col)
  }
  message('Data source: Google Trends (https://www.google.com/trends).')
  return(df)
}
