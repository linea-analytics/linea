#' apply_normalisation
#' 
#' Normalise data based on pool mean
#' 
#' Normalise data by dividing all values in each pool by that pool's mean
#' 
#' @export
#' @import tidyverse
#' @import tibble
#' @import gtrendsR
#' @importFrom anytime anydate 
#' @param data \code{data.frame} containing data for analysis
#' @param kw a string of the search keyword
#' @param date_col a string specifying the date column name
#' @param date_type The date column type as either of the following strings:'weekly starting','weekly ending','daily'
#' @param geo a string specifying the country code of the search found in \code{countrycode::codelist}
#' @param append a boolean specifying whether to return the original data.frame as well as the added column
#' @return \code{data.frame} of the original data with the added google trend column
#' @examples 
#' data = read_xcsv("https://raw.githubusercontent.com/paladinic/data/main/ecomm_data.csv") %>% 
#'   gt_f(kw = 'covid') %>% 
#'   gt_f(kw = 'bitcoin')
#' 
gt_f = function(data,
                kw,
                date_col = "date",
                date_type = "weekly starting",
                geo = "all",
                append = FALSE) {
  # get gtrends data    ####
  
  # get dates from data
  dates = data %>% pull(sym(date_col)) %>% as.Date() %>% unique()
  
  
  min_date = min(dates) %>% anytime::anydate(tz = 0)
  max_date = max(dates) %>% anytime::anydate(tz = 0)
  
  if(max_date > Sys.Date()){
    max_date = Sys.Date()
  }
  
  # create time string for gtrends request
  time_str = paste0(min_date, " ", max_date)
  
  # make gtrends request
  if(geo=="all"){
    gt = gtrends(
      keyword = kw,
      time = time_str,
      onlyInterest = TRUE
    )
  }
  else{
    gt = gtrends(
      keyword = kw,
      geo = geo,
      time = time_str,
      onlyInterest = TRUE
    )}
  
  
  
  gt = gt[[1]]%>%
    select(date, hits) %>%
    tibble()
  
  # save gtrends dates vector
  gt_dates = gt$date
  
  # get gtrends date type
  if(is_daily(gt$date))gt_date_type = "daily"
  if(is_weekly(gt$date))gt_date_type = "weekly starting"
  
  # get daily gtrends   ####
  
  # create daily dates depending on date_type
  extremes = gt_dates %>% as.Date() %>% first_last_dates(date_type = gt_date_type)
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
  # get data daily map  ####
  
  
  # create daily dates depending on data date_type
  extremes = dates %>% as.Date() %>% first_last_dates(date_type = date_type)
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
    summarise(hits = mean(hits)) %>%
    ungroup()
  
  
  colnames(df)[1] = date_col
  colnames(df)[2] = paste0("gtrends_",kw)
  
  if(append){
    df = data %>% left_join(df,by=date_col)
  }
  return(df)
}
