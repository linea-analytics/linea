#' is_uniform_ts
#'
#' Check if a time-series is uniform
#'
#' Check if a time-series is uniform, where the step (e.g. days(1),weeks(7)) is consistent
#'
#' @param dates a date-type or numeric vector
#' @return boolean to specify whether the time series is uniform
#' @export
is_uniform_ts = function(dates){
  d = diff(dates)
  return(all(d == mean(d)))
}

#' is_daily
#'
#' Check if a time-series is daily
#'
#' Check if a time-series is daily return boolean
#'
#' @param dates a date-type or numeric vector
#' @return boolean to specify whether the time series has a daily frequency
#' @export
is_daily = function(dates){
  
  # unique dates for pooled
  dates = unique(dates)
  
  mean_diff = mean(dates[-1] - dates[1:(length(dates)-1)])

  if(mean_diff == 1){TRUE}
  else{FALSE}

}

#' is_weekly
#'
#' Check if a time-series is weekly
#'
#' Check if a time-series is weekly return boolean
#'
#' @param dates a date-type or numeric vector
#' @return boolean to specify whether the time series has a weekly frequency
#' @export
is_weekly = function(dates){

  # unique dates for pooled
  dates = unique(dates)
  
  mean_diff = mean(dates[-1] - dates[1:(length(dates)-1)])
  if(mean_diff == 7){TRUE}
  else{FALSE}

}

#' first_last_dates
#'
#' Check if a time-series is uniform
#'
#' Check if a time-series is uniform, where the step (e.g. days(1),weeks(7)) is consistent
#'
#' @param date_values a date-type or numeric vector
#' @param date_type The date column type as either of the following strings:'weekly starting','weekly ending','daily'
#' @return list of first and last daily dates
#' @export
first_last_dates = function(date_values,date_type){
  if(date_type == "weekly starting"){
    first_date = date_values[1]
    last_date = date_values[length(date_values)]+6
  }
  if(date_type == "weekly ending"){
    first_date = date_values[1]-6
    last_date = date_values[length(date_values)]
  }
  if(date_type == "daily"){
    first_date = date_values[1]-6
    last_date = date_values[length(date_values)]
  }
  return(list(first_date = first_date,
              last_date = last_date))
}

#' check_ts
#'
#' Check time series dataframe
#'
#' Check if dataframe contains specified date column and that its data-type
#'
#' @param data The dataframe containing the column specified
#' @param date_col The date column name as a string
#' @param allow_non_num A boolean to specify whether to include only date and numeric columns
#' @param verbose A boolean to specify whether to print warnings
#' @return checked \code{data.frame}
#' @export
#' @importFrom zoo as.Date
#' @importFrom lubridate is.POSIXct
#' @import dplyr
check_ts = function(data,
                    date_col,
                    allow_non_num = TRUE,
                    verbose = FALSE){

  if(!is.logical(verbose)){
    message("verbose must be logical (TRUE or FALSE). Setting to TRUE.")
    verbose = TRUE
  }
  
  if(verbose){
    message("Checking time-series")
  }
  
  ## contains date column
  if(!is.character(date_col)){
    if(verbose)message("- Error: date_col must be of type character.")
    return(NULL)
  }
  if(!(date_col %in% colnames(data))){
    if(verbose)message("- Error: date_col not found in data.")
    return(NULL)
  }

  date_type = class(data %>% pull(!!sym(date_col)))

  if(any(date_type == c("POSIXct","POSIXt"))){
    date_type = "Date"
    data[, date_col] = data %>%
      pull(!!sym(date_col)) %>%
      as.Date()
  }
  if(date_type != "Date"){
    if(verbose)message("- Error: date_col provided is not of type date.")
    return(NULL)
  }

  if(!allow_non_num){
    ## contains date and numeric cols only
    other_types = sapply(data,function(x)is.numeric(x)|is.POSIXct(x))

    if(!all(other_types)){
      if(verbose)message("- Warning: Non-numeric or non-date variables have been dropped.")
      return(data[,other_types])
    }
  }

  return(data)
}


#' get_seasonality
#'
#' generate seasonality variables
#'
#' generate seasonality variables from a \code{data.frame} containing a date-type variable.
#'
#' @param data \code{data.frame} containing data for analysis
#' @param date_col_name The date column name as a string
#' @param date_type The date column type as either of the following strings:'weekly starting','weekly ending','daily'
#' @param date_format The format of th date column as a string (e.g. "%d/%m/%Y")
#' @param verbose A boolean to specify whether to print warnings
#' @param keep_dup A boolean to specify whether to keep duplicate columns between seasonal and data
#' @param pool_var The pool (group) column name as a string (e.g. 'country')
#' @return \code{data.frame} with added variables
#' @export
#' @importFrom sjmisc to_dummy
#' @importFrom tis isEaster isGoodFriday
#' @importFrom lubridate is.Date
#' @import tidyverse
#' @examples
#' linea::sales_ts %>%
#'    get_seasonality(date_col_name = 'week')
get_seasonality = function(data,
                           date_col_name,
                           date_type = NULL,
                           date_format = '%d/%m/%Y',
                           verbose = FALSE,
                           keep_dup = FALSE,
                           append = TRUE,
                           pool_var = NULL){
  # test    ####
  
  # data = linea::sales_ts
  # date_col_name = 'week'
  # date_type = 'weekly starting'
  # date_format = NULL
  # verbose = TRUE
  # keep_dup = FALSE
  # pool_var = NULL
  
  # data = linea::pooled_gt_data
  # date_col_name = 'Week'
  # # date_type = 'weekly starting'
  # date_type = NULL
  # # date_format = '%d/%m/%Y'
  # date_format = NULL
  # verbose = TRUE
  # keep_dup = FALSE
  # pool_var = NULL
  # # pool_var = "country"
  
  # data = linea::cran_downloads
  # date_col_name = 'date'
  # date_type = NULL
  # date_format = NULL
  # verbose = TRUE
  # keep_dup = FALSE
  # pool_var = NULL
  
  
  # checks  ####
  # check verbose
  if(!is.logical(verbose)){
    message("verbose must be logical (TRUE or FALSE). Setting to TRUE.")
    verbose = TRUE
  }
  
  if(verbose){
    message("Generating seasonality variables...")
  }
  
  # check data provided is a data.frame
  if(!is.data.frame(data)){
    message('- Error: data must be a dataframe. Returning NULL.')
    return(NULL)
  }
  # check data provided contains date_col_name
  if(!(date_col_name %in% colnames(data))){
    message('- Error: data must contain date_col_name. Returning NULL.')
    return(NULL)
  }
  date_col = pull(data,date_col_name) 
  # check pool
  if(!is.null(pool_var)){
    if(!(pool_var %in% colnames(data))){
      if(verbose)message('- Warning: data must contain pool_var. Setting pool_var to NULL.')
      pool_var = NULL
    }
  }
  # check date_type
  if(is.null(date_type)){
    if(is_weekly(dates = date_col)){
      date_type = 'weekly startning'
      if(verbose){message('- Info: data_type inferred as weekly (starting).')}
    }else if(is_daily(dates = date_col)){
      date_type = 'daily'
      if(verbose){message('- Info: data_type inferred as daily.')}
    }
    else{
      message('- Error: data_type not provided and could not be inferred. Returning NULL.')
      return(NULL)
    }
  }
  
  if(!(date_type %in% c('weekly starting', "weekly ending", "daily"))){
    if(verbose)message('- Warning: date_type must be either "weekly starting", "weekly ending", "daily". Setting date_type to "weekly starting".')
    date_type = "weekly starting"
  }
  
  
  if(!lubridate::is.Date(date_col)){
    date_col = as.Date(x = date_col,format = date_format)
    if(any(is.na(date_col))){
      message("- Error: could not convert date column to date type. 
            Check the `data` and `date_format` provided. Returning NULL.")
      return(NULL)
    }
    
    data[date_col_name] = date_col
    
  } 
  
  # process ####
  
  # get dates column
  date_values = data %>%
    pull(!!sym(date_col_name)) %>%
    unique() %>% 
    sort()
  
  # define start and end date
  if(date_type == "daily"){
    first_date = date_values[1]
    last_date = date_values[length(date_values)]
  }
  if(date_type == "weekly starting"){
    first_date = date_values[1]
    last_date = date_values[length(date_values)]+6
    
    # build date vector
    date_values = seq(first_date,last_date,by = 1)
  }
  if(date_type == "weekly ending"){
    first_date = date_values[1]-6
    last_date = date_values[length(date_values)]
    
    # build date vector
    date_values = seq(first_date,last_date,by = 1)
  }
  
  
  # build dataframe of weeks 1 to 52
  week_df = tibble(day = date_values) %>%
    mutate(week_num = strftime(day, format = "%V"))
  
  week_df = week_df %>%
    left_join(by="day",
              week_df %>%
                sjmisc::to_dummy(week_num) %>%
                mutate(day = date_values) %>%
                data.frame() %>%
                tibble()) %>% 
    mutate(week_num = paste0("w ",week_num))
  
  # build dataframe of months 1 to 12
  month_df = tibble(day = date_values) %>%
    mutate(month = strftime(day, format = "%b")) 
  
  month_df = month_df %>%
    left_join(by="day",
              month_df %>% 
                sjmisc::to_dummy(month,suffix = "label") %>%
                mutate(day = date_values) %>%
                data.frame() %>%
                tibble())
  
  # build dataframe of years in data
  year_df = tibble(day = date_values) %>%
    mutate(year = strftime(day, format = "%Y"))
  
  year_df = year_df %>%
    left_join(by="day",
              year_df %>% 
                sjmisc::to_dummy(year,suffix = "label") %>%
                mutate(day = date_values) %>%
                data.frame() %>%
                tibble()) %>% 
    mutate(year = paste0("y",year))
  
  # build dataframe of events e.g. xmas
  events_df = tibble(day = date_values) %>%
    mutate(christmas_day = if_else(strftime(day,format = "%d-%m") == "25-12",1,0)) %>%
    mutate(christmas_eve = if_else(strftime(day,format = "%d-%m") == "24-12",1,0)) %>%
    mutate(new_years_day = if_else(strftime(day,format = "%d-%m") == "01-01",1,0)) %>%
    mutate(new_years_eve = if_else(strftime(day,format = "%d-%m") == "31-12",1,0)) %>%
    mutate(easter = as.integer(isEaster(day))) %>%
    mutate(good_friday = as.integer(isGoodFriday(day)))
  
  
  # join dataframes
  events_df = events_df %>%
    left_join(week_df,by="day") %>%
    left_join(month_df,by="day") %>%
    left_join(year_df,by="day")
  
  
  if(date_type %in% c("weekly starting", "weekly ending")){
    
    # get week dates
    week_dates = data %>%
      pull(!!sym(date_col_name)) %>%
      unique()
    
    # recreate week dates using first and last to avoid missing weeks
    
    first_date = week_dates[1]
    last_date = week_dates[length(week_dates)]
    week_dates = seq(first_date,last_date,by = 7) %>%
      rep(each=7)
    
    # aggregation (daily to weekly)
    df = data.frame(day = date_values,
                    week = week_dates) %>%
      left_join(events_df, by = "day")
    
    
    df_cat = df %>% 
      select(where(is.character))%>%
      mutate(week = week_dates)
    
    df_cat = df_cat %>% 
      group_by(week) %>%
      summarise_all(first)
    
    df = df %>%
      select(-week_num,-month,-year) %>% 
      group_by(week) %>%
      summarise_all(mean) %>% 
      left_join(df_cat, by="week")
    
    df$day = NULL
    
    colnames(df)[colnames(df) == 'week'] = date_col_name
    
  }
  if(date_type == 'daily'){
    
    weekdays_weekends = tibble(day = date_values) %>% 
      mutate(weekday = weekdays(day)) %>% 
      to_dummy(weekday,suffix = 'label') %>%
      mutate(day = date_values) %>% 
      mutate(weekend = 0)
    
    if("weekday_Saturday" %in% colnames(weekdays_weekends)){
      weekdays_weekends = weekdays_weekends %>% 
        mutate(weekend = if_else(weekday_Saturday == 1,1,weekend))
    }
    
    if("weekday_Sunday" %in% colnames(weekdays_weekends)){
      weekdays_weekends = weekdays_weekends %>% 
        mutate(weekend = if_else(weekday_Sunday == 1,1,weekend))
    }
    
    df = data.frame(day = date_values) %>%
      left_join(events_df, by = "day") %>% 
      left_join(weekdays_weekends, by = "day") %>% 
      mutate(weekday = weekdays(day))
    
    colnames(df)[colnames(df) == 'day'] = date_col_name
    
  }
  
  if (!is.null(pool_var)) {
    data = data %>%
      mutate(number = 1) %>%
      group_by(!!sym(pool_var)) %>%
      mutate(trend = cumsum(number)) %>%
      select(-number) %>% 
      ungroup() 
  }else{
    data = data %>%
      mutate(number = 1) %>%
      mutate(trend = cumsum(number)) %>%
      select(-number)
  }
  
  # columns check
  
  new_cols = colnames(df)
  data_cols = colnames(data)
  
  dup_columns = new_cols[new_cols %in% data_cols]
  dup_columns = dup_columns[dup_columns!=date_col_name]
  
  if(!keep_dup){
    df[,dup_columns] = NULL
  }
  
  data = data %>%
    left_join(df,
              by = date_col_name,
              suffix = c("", ".x"))
  
  if(!append){
    
    data_cols = data_cols[data_cols != date_col_name & data_cols != "trend"]
    
    data = data %>% 
      select(-all_of(data_cols))
    
  }
  
  if(verbose){
    message("Seasonality variables have been generated.")
  }
  
  return(data)
}

