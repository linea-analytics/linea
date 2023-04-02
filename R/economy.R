#' get_economy
#'
#' Generate economy variables
#'
#' Generate economy variables from a \code{data.frame} containing a date-type variable.
#' The function will collect data from the world bank based on the country_code input.
#' Appropriate country codes can be viewed using the tibble \code{linea::countries$iso2c}.
#'
#' @param data \code{data.frame} containing data for analysis
#' @param date_col_name The date column name as a string
#' @param country_code A string indicating which county's economy data to retrieve. 
#' @param append A boolean to specify whether to return only the economy variables or the original input data plus the economy variables
#' @param verbose A boolean to specify whether to print warnings 
#' @return \code{data.frame} with added variables
#' @export
#' @importFrom lubridate year
#' @importFrom tidyr gather fill
#' @import tidyverse
#' @examples
#' linea::sales_ts %>%
#'    get_economy(date_col_name = 'week',country_code = 'US',append=FALSE)
get_economy = function(data,
                       country_code = NULL,
                       date_col_name = NULL,
                       append = TRUE,
                       verbose = TRUE){
  # test    ----
  
  # data = sales_ts
  # # data = read_xcsv('/Users/44751/Desktop/sales_2023-04-01 (1).xlsx')
  # date_col_name = "week"
  # # pool_var = NULL
  # country_code = 'GB'
  # # country_code = 'AW'
  # # country_code = 'ZH'
  # verbose = TRUE

  # POOL VAR
  # data = pooled_gt_data %>% 
  #   mutate(country = if_else(country == 'UK','GB',country))
  # date_col_name = "Week"
  # pool_var = 'country'
  # country_code = NULL
  # verbose = TRUE
  
  
  # checks  -----------------------------------------------------------------
  
  # check verbose
  if(!is.logical(verbose)){
    message("verbose must be logical (TRUE or FALSE). Setting to TRUE.")
    verbose = TRUE
  }
  
  # check date_col_name
  if(is.null(date_col_name)){
    message('Error: A `date_col_name` must be provided. Returning NULL.')
    return(NULL)
  }
  
  # # if both pool_var and country_code are NULL
  # if(is.null(pool_var) & is.null(country_code)){
  #   message('Warning: Either `pool_var` and `country_code`. Returning `data`.')
  #   return(data)
  # }
  
  # if both pool_var and country_code are provided
  # if(!is.null(pool_var) & !is.null(country_code)){
  #   if(verbose){
  #     message('Warning: Both `pool_var` and `country_code` have been provided. `pool_var` will be ignored.')
  #   }
  #   pool_var = NULL
  # }
  
  # # if pool_var in data
  # if(!is.null(pool_var)){
  #   if(!pool_var %in% colnames(data)){
  #     message('Error: `pool_var` not found in `data`. Returning `data`. ')
  #     return(data)
  #   }
  # }
  
 
  
  # if country_code in world_bank_countries
  if(!is.null(country_code)){
    
    world_bank_countries = linea::countries %>% 
      gather() %>% 
      pull(value)
    
    if(!country_code %in% world_bank_countries){
      message('Error: `country_code` not found. 
              Use the function `linea::countries` to validate your `country_code` input. 
              Returning `NULL`.')
      return(NULL)
    }
  }
  
  
  # process -----------------------------------------------------------------
  
  # get date variable for start and end
  dates = data %>% 
    pull(date_col_name)
  
  start = min(dates) %>% lubridate::year()
  end = max(dates) %>% lubridate::year()
  
  econ_cols = c("gdp_capita","unemp","pop")
  new_cols = paste0(country_code,'_',econ_cols)
  econ_data = linea::wb_data %>% 
    filter(iso2c == country_code) %>% 
    select(all_of(c("date",econ_cols)))
  
  if(any(colnames(data) %in% new_cols)){
    for(col in new_cols){
      if(col %in% colnames(data)){
        data = data %>% 
          select(-all_of(col))
        if(verbose)message("Warning: variable ",col," has been updated with new values.")
      }
    }
  }
  
  data = data %>%
    mutate(temp_year_var_for_join = lubridate::year(!!sym(date_col_name))) %>%
    left_join(econ_data,by = c('temp_year_var_for_join'='date')) %>%
    select(-temp_year_var_for_join)
  
  all_cols = colnames(data)
  all_cols[all_cols %in% econ_cols] = new_cols
  colnames(data) = all_cols
  
  # replace inf with NA i
  if(any(is.na(data[new_cols] == Inf | data[new_cols] == -Inf))){
    if(verbose)message("Warning: Infinite value found in economy data will be turned to NA.")
    data = do.call(tibble,lapply(data, function(x) replace(x, is.infinite(x),NA)))
  }
  
  # any years missing?
  if(any(is.na(data[new_cols]))){
    
    ## which dates are missing?
    # missing_dates = data[is.na(data[new_cols])[,1],] %>% 
    #   pull(!!sym(date_col_name))
    # missing_dates_start = min(missing_dates)
    # missing_dates_end = max(missing_dates)
    #
    # message("Warning: missing economy data from ",missing_dates_start," to ",missing_dates_end,". 
    #         Filling missing economy variables with latest values.")
    
    for(i in 1:length(new_cols)){
      
      new_col = new_cols[i]
      new_col_data = data[new_col]
      
      if(all(is.na(new_col_data))){
        if(verbose)message("Warning: data missing completely for variable ",new_col,". The variable will be dropped.")
        data[new_col] = NULL
      }else if(any(is.na(new_col_data))){
        if(verbose)message("Warning: data missing for variable ",new_col,". Missing data filled with latest values.")
        data[new_col] = tidyr::fill(new_col_data,.direction = 'down')
      }
    }
  }
  
  if(append){
    return(data)
  }else{
    
    new_cols = new_cols[new_cols %in% colnames(data)]
    
    return(data %>% 
             select(all_of(c(date_col_name,new_cols))))
  }
  
  # For API
  # # world bank indicators
  # indicators = c(
  #   gdp_capita ="NY.GDP.PCAP.CD",
  #   pop = "SP.POP.TOTL",
  #   unemp = "SL.UEM.TOTL.ZS"
  # )
  # 
  # # get world_bank_data using country_code
  # if(!is.null(country_code)){
  #   
  #   world_bank_data = wb_data(
  #     country = country_code,
  #     indicator = indicators, 
  #     start_date = start, 
  #     end_date = end) %>% 
  #     select(-iso3c,-country,-iso2c) %>%
  #     TRY(verbose = T)
  #   
  #   # check if API failed
  #   if(is.null(world_bank_data)){
  #     message('Error: Failed to retrieve world bank data. 
  #     Check your internet connection and the input arguments, or try again later. 
  #     Returning `data` without economy variables.')
  #     return(data)
  #   }
  #   
  #   colnames(world_bank_data) = c("date",paste0(country_code,'_',c("gdp_capita","unemp","pop")))
  #   
  #   # add year to data
  #   data = data %>% 
  #     mutate(temp_year_var_for_join = year(!!sym(date_col_name))) %>% 
  #     left_join(world_bank_data,by = c('temp_year_var_for_join'='date')) %>% 
  #     select(-temp_year_var_for_join)
  #     
  #   return(data)
  #   
  # }

  # get world_bank_data using pool_var
  # if(!is.null(pool_var)){
  #   
  #   # get countries in pool_var
  #   countries = data %>% 
  #     pull(!!sym(pool_var)) %>% 
  #     unique()
  #   
  #   # get world_bank_data
  #   world_bank_data = wb_data(
  #     country = countries,
  #     indicator = indicators, 
  #     start_date = start, 
  #     end_date = end) %>% 
  #     TRY()
  #   
  #   # check if API failed
  #   if(is.null(world_bank_data)){
  #     message('Error: Failed to retrieve world bank data. Check your internet connection and the input arguments. Returning `data`.')
  #     return(data)
  #   }
  #   
  #   # check if any country is missing
  #   countries_in_wb_data = world_bank_data %>% 
  #     pull(country) %>% 
  #     unique()
  #   
  #   if(length(countries_in_wb_data) != length(countries)){
  #     message('Error: Not all countries in `pool_var` could be retrieved. Returning `data`.')
  #     return(data)
  #   }
  #   
  #   
  #   # allign country column
  #   wb_pool_var_for_join = c()
  #   
  #   # for each country in the pool_var
  #   for(country in countries){
  #     
  #     # check each country column in the wb data
  #     for(i in 1:3){
  #       
  #       # if there is a match
  #       if(any(world_bank_data[i] == country)){
  #         
  #         # append that to join column
  #         wb_pool_var_for_join = c(
  #           wb_pool_var_for_join,
  #           world_bank_data[i][world_bank_data[i] == country]
  #           )
  #         
  #         break
  #         
  #       }
  #     }
  #   }
  #   
  #   world_bank_data[pool_var] = wb_pool_var_for_join
  #   
  #   # add year to data
  #   data = data %>% 
  #     mutate(temp_year_var_for_join = year(!!sym(date_col_name))) %>% 
  #     left_join(world_bank_data[c(pool_var,'date',"gdp_capita","unemp","pop")],
  #               by = c('temp_year_var_for_join'='date',pool_var)) %>% 
  #     select(-temp_year_var_for_join)
  # 
  #   return(data)
  #   
  # }
}
