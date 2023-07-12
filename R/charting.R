#' color_palette
#'
#' A pre-loaded color palette
#'
#' A pre-loaded color palette that can be used in charting functions
#'
#' @export
#' @return character vector of colors in hexadecimal notation
color_palette = function(){
  return(
    list(
      decomp_color_1 = "#636363",
      decomp_color_2 = "#143fba",
      decomp_color_3 = "#006a53",
      decomp_color_4 = "#209162",
      decomp_color_5 = "#33eea0",
      decomp_color_6 = "#8f9d35",
      decomp_color_7 = "#00cf74",
      decomp_color_8 = "#b400da",
      decomp_color_9 = "#fff200",
      decomp_color_10 = "#ff5c00",
      decomp_color_11 = "#6a000d",
      decomp_color_12 = "#ff006e",
      charts_font_color = '#1c0022',
      charts_background_color = "#FFFFFF",
      charts_zero_line_color = '#1c0022',
      resid_line_color = "#C90F3A",
      actual_line_color = '#1c0022',
      predicted_line_color = "#00cf74",
      resid_bar_color = "#151c1f",
      acf_bar_color = "#151c1f" 
    )
  )
}

#' decomping
#'
#' Variable decomposition of linear regression
#'
#' Calculates the decomposition of the independent variables based on an input model object.
#' This can be expanded by leveraging id variables (e.g. date) and categories (i.e. groups of variables).
#'
#' @export
#' @param model Model object
#' @param de_normalise A boolean to specify whether to apply the normalisation
#' @param categories \code{data.frame} mapping variables to groups
#' @param tail_window for time series, length of tail
#' @param verbose A boolean to specify whether to print warnings
#' @import tidyverse
#' @importFrom stats complete.cases
#' @importFrom methods is
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr '%>%'
#' @importFrom reshape2 melt acast
#' @return a \code{list} of 3 \code{data.frame}'s representing the variable and category decomposition, and the fitted values.
#' @examples
#' run_model(data = mtcars,dv = 'mpg',ivs = c('wt','cyl','disp'),decompose=FALSE) %>% decomping()
decomping = function(model = NULL,
                     de_normalise = TRUE,
                     categories = NULL,
                     tail_window = NULL,
                     verbose = FALSE){
  # test    ####
  
  # raw_data = pooled_gt_data
  # dv = "amazon"
  # ivs = c("rakhi", "christmas", "diwali")
  # id_var = "Week"
  # pool_var = 'country'
  # 
  # model_table = build_model_table(c(ivs, "", ""))
  # model = run_model(
  #   id_var = id_var,
  #   verbose = FALSE,
  #   data = raw_data,
  #   dv = dv,
  #   pool_var = pool_var,
  #   model_table = model_table,
  #   normalise_by_pool = TRUE
  # )
  # 
  # rm(raw_data)
  # rm(model_table)
  # rm(pool_var)
  # rm(ivs)
  # rm(id_var)
  # rm(dv)
  # 
  # de_normalise = FALSE
  # de_normalise = TRUE
  # categories = NULL
  # verbose = TRUE
  # tail_window = 10

  # checks  ####
  
  # check verbose
  if(!is.logical(verbose)){
    message("Warning: verbose provided mus be logical (TRUE or FALSE). Setting to TRUE.")
    verbose = TRUE
  }
  
  if(verbose)message("Generating model decomposition...")
  
  
  # check if model is provided
  if(is.null(model)){
    if(verbose){
      message("- Error: No model provided. Returning NULL.")
    }
    return(NULL)
  }
  if(!is(model,'lm')){
    if(verbose){
      message("- Error: model must be of type 'lm'. Returning NULL.")
    }
    return(NULL)
  }
  
  
  # get the coefficients from the model object
  coef = model$coefficients
  ivs = model$model_table %>% 
    filter(variable_t != '') %>% 
    pull(variable_t)
  
  
  # TODO: function for checking model object to use in all functions
  # if(!("model_table" %in% names(model))){
  #   model_table = build_model_table(ivs = ivs,trans_df = model$trans_df) %>% 
  #     get_variable_t()
  # }
  
  # extract dependent variable name
  dv = model$dv
  
  # get the modeled data from the model object
  data = model$model
  raw_data = model$raw_data

  # check raw_data for and drop NAs
  if(any(!complete.cases(raw_data))) {
    if (verbose) {
      message("- Warning: NA's found in raw data will be replaced with zeros.")
    }
    # raw_data = raw_data[complete.cases(raw_data),]
    raw_data[raw_data %>% is.na()] = 0
  }
  
  # get the dependent variable from the data object
  actual = data %>% pull(!!sym(dv))
  raw_actual = raw_data %>% pull(!!sym(dv))
  
  if(!is.logical(de_normalise)){
    if(verbose){
      message("- Warning: de_normalise provided must be of type logical. Setting de_normalise to FALSE.")
    }
    de_normalise = FALSE
  }
  
  
  # get the intercept value from the coef object
  intercept = coef[1]
  # keep the other coefficients
  coef = coef[-1]
  
  
  # offset
  if(any(model$model_table$fixed!='')){
    
    fixed_vars = model$model_table %>% 
      filter(fixed != '') %>% 
      pull(variable_t)
    
    fixed_coefs = rep(1,length(fixed_vars))
    
    names(fixed_coefs) = fixed_vars
    
    coef = c(coef,fixed_coefs)
  }
  
  
  # get pool var
  pool_var = model$pool_var
  pool_var_values = raw_data %>% pull(!!sym(pool_var))
  
  # get id_var and values
  id_var = model$id_var
  id_var_values = raw_data %>% pull(!!sym(id_var))
  
  # process ####
  
  # generate the fitted values dataframe
  fitted_values = tibble(
    actual = c(actual),
    residual = model$residuals,
    predicted = model$fitted.values,
    id = id_var_values, #%>% factor(),
    pool = pool_var_values # %>% factor()
  ) 
  fitted_values = pivot_longer(
    data = fitted_values,
    cols = c('actual','residual','predicted'),
    values_to = 'value',
    names_to = "variable") %>% 
    arrange(variable,id)
  
  
  if(!is.null(tail_window) & !is.null(raw_data)){
    # extend id
    
    ## get id
    unique_id_var_values  = id_var_values %>%
      unique() %>%
      sort()
    
    ## get interval (mode of diff)
    interval = diff(unique_id_var_values)
    uniqv = unique(interval)
    interval = uniqv[which.max(tabulate(match(interval, uniqv)))]
    
    ## generate id extension
    start = max(unique_id_var_values) + interval
    end = max(unique_id_var_values) + (interval * tail_window)
    id_ext = seq(start, end, interval)
    
    #TODO:
    # WARNING # what if the id is not consistent across pools (i.e. different ends)?
    
    ## get pool
    unique_pool = pool_var_values %>%
      unique()
    
    # blank df with extended index and pool
    
    ## combos dates, pools
    df_ext = expand.grid(id_ext, unique_pool) %>%
      data.frame()
    colnames(df_ext) = c(id_var, pool_var)
    
    ## variables' columns
    
    raw_ivs = model$model_table$variable %>%
      unique() %>%
      {
        .[. != '']
      }
    
    df_ext[raw_ivs] = 0
    
    raw_data[id_var] = id_var_values
    
    # append df
    independendent_variables = apply_transformation(
      raw_data = raw_data %>%
        bind_rows(df_ext) %>%
        select(id_var, pool_var, raw_ivs),
      model_table = model$model_table,
      trans_df = model$trans_df,
      pool_var = model$pool_var,
      verbose = verbose
    ) %>%
      rename(id = !!sym(id_var),
             pool = !!sym(pool_var)) #%>%
    
    id_ext = independendent_variables %>% 
      pull(id)
    pool_ext = independendent_variables %>% 
      pull(pool)
    independendent_variables = independendent_variables %>% 
      select(all_of(names(coef)))
    
    
    if (length(coef) == 1) {
      # multiply independent variable by coefficient
      variable_decomp = data.frame(independendent_variables * coef)
      colnames(variable_decomp) = names(coef)
    } else{
      # multiply independent variables data frame by coefficient vector
      variable_decomp = data.frame(mapply(
        FUN = `*`,
        independendent_variables,
        coef,
        SIMPLIFY = FALSE
      ))
    }
    
    # rename variable decomp using coef names
    colnames(variable_decomp) = names(coef)
    
    variable_decomp = tibble(
      "(Intercept)" = intercept,
      variable_decomp,
      id = id_ext,
      pool = pool_ext
    )
    
  }else{
    
    # check raw_data & tail_window
    if(!is.null(tail_window) & is.null(raw_data)){
      
      if(verbose)message('- Warning: No raw_data supplied for tail_window. Ignoring tail window.')
      
    }
    
    # get the independent variables decomp
    independendent_variables =  data[, 2:ncol(data)]
    if (length(coef) == 1) {
      # multiply independent variable by coefficient
      variable_decomp = data.frame(independendent_variables * coef)
      colnames(variable_decomp) = names(coef)
    } else{
      # multiply independent variables data frame by coefficient vector
      variable_decomp = data.frame(mapply(
        FUN = `*`,
        independendent_variables,
        coef,
        SIMPLIFY = FALSE
      ))
    }
    
    # rename variable decomp using coef names
    colnames(variable_decomp) = names(coef)
    
    variable_decomp = tibble(
      "(Intercept)" = intercept,
      variable_decomp,
      id = id_var_values,
      pool = pool_var_values
    )
  }
  
  
  # generate tibble df using the variable decomp, intercept and id variable
  variable_decomp = variable_decomp %>% 
    pivot_longer(cols = c('(Intercept)',names(coef)),
                 names_to = 'variable',
                 values_to = 'value') %>% 
    arrange(variable,id) %>% 
    rename(contrib = value)
  
  # if an id variable name is provided use it
  if (id_var != "id") {
    # fitted values
    col_names = colnames(fitted_values)
    col_names[col_names == "id"] = id_var
    colnames(fitted_values) = col_names
    
    # decomp
    col_names = colnames(variable_decomp)
    col_names[col_names == "id"] = id_var
    colnames(variable_decomp) = col_names
    
  }
  
  # if a raw actual is provided and de-normalise is TRUE, check if dv is STAed
  if(de_normalise){
    
    pool_mean = tibble(raw_actual = raw_actual,
                       pool = pool_var_values) %>%
      group_by(pool) %>%
      summarise(pool_mean = mean(raw_actual))
    
    
    variable_decomp = variable_decomp %>%
      left_join(pool_mean, by = "pool") %>%
      mutate(contrib = contrib * pool_mean) %>%
      select(-pool_mean)
    
    
    fitted_values = fitted_values %>%
      left_join(pool_mean, by = "pool") %>%
      mutate(value = value * pool_mean) %>%
      select(-pool_mean)
  }
  
  # IMPROVE CATEGORIES CHECK?
  if (is.null(categories)) {
    if(all(model$model_table$category == "")){
      if(verbose){
        message("- Warning: No categories table provided and no categories found in model_table. Setting category_decomp = variable_decomp.")
      }
      category_decomp = variable_decomp
    }else{
      # create category from model table categories
      categories = model$model_table %>%
        select(variable,category) %>%
        mutate(category = if_else(category == '','Other',category)) %>%
        mutate(calc = 'none')
    }
  } else if(!is.data.frame(categories)){
    if(all(model$model_table$category == "")){
      if(verbose){
        message("- Warning: categories table provided is not a data.frame and no categories found in model_table. Setting category_decomp = variable_decomp.")
      }
      category_decomp = variable_decomp
    }else{
      
      if(verbose){
        message("- Warning: categories provided must be of type data.frame. Using model_table categories.")
      }
      # create category from model table categories
      categories = model$model_table %>%
        select(variable,category) %>%
        mutate(category = if_else(category == '','Other',category)) %>%
        mutate(calc = 'none')
    }
  }else if(!(all(c('variable','category') %in% colnames(categories)))){
    if(verbose){
      message("- Warning: categories provided must contain atleast columns 'variable' and 'category'.")
    }
    if(all(model$model_table$category == "")){
      if(verbose){
        message("- Warning: categories table provided is not a data.frame and no categories found in model_table. Setting category_decomp = variable_decomp.")
      }
      category_decomp = variable_decomp
    }else{
      
      if(verbose){
        message("- Warning: categories provided must be of type data.frame. Using model_table categories.")
      }
      # create category from model table categories
      categories = model$model_table %>%
        select(variable,category) %>%
        mutate(category = if_else(category == '','Other',category)) %>%
        mutate(calc = 'none')
    }
  }
  
  if(!exists('category_decomp')){
    
    if(!('calc' %in% colnames(categories))){
      if(verbose){
        message("- Warning: categories type data.frame provided does not include a 'calc' column. Setting all categories to 'none'.")
      }
      categories$calc = 'none'
    }
    
    # generate category decomp using categories df input
    category_decomp = variable_decomp %>%
      # add a categories and calc column to the variables decomp table
      left_join(categories, by = "variable") %>%
      # if no category is found for a variable assign it "Other" as a category
      mutate(category  = if_else(is.na(category),
                                 "Other",
                                 category)) %>%
      # assign the "Base" category to the intercept variable
      mutate(category  = if_else(variable == "(Intercept)",
                                 "Base",
                                 category)) %>%
      # if no calc is found for a variable/category assign it "none" as a calc
      mutate(calc  = if_else(is.na(calc),
                             "none",
                             calc)) %>%
      # group and sum the table by id and category
      group_by(!!sym(id_var), category,pool) %>%
      summarise(contrib = sum(contrib)) %>%
      rename(variable = category)
    
    # extract minned variables
    minned_vars = categories %>%
      filter(calc == "min") %>%
      pull(category) %>%
      unique()
    
    # extract maxed variables
    maxed_vars = categories %>%
      filter(calc == "max") %>%
      pull(category) %>%
      unique()
    
    # extract the initial (pre calc) base value
    based_value = category_decomp[category_decomp$variable == "Base", "contrib"]
    
    # for each minned variable
    for (cat in minned_vars) {
      # get the category values
      cat_val = category_decomp %>%
        filter(variable == cat) %>%
        pull(contrib)
      
      # get the minimum of each category
      min_val = cat_val %>%
        min()
      
      # replace the category values with the minned variable
      category_decomp[category_decomp$variable == cat, "contrib"] = cat_val - min_val
      
      # replace the base value with the base plus min value
      based_value = based_value + min_val
    }
    # for each maxed variable
    for (cat in maxed_vars) {
      # get the category values
      cat_val = category_decomp %>%
        filter(variable == cat) %>%
        pull(contrib)
      
      # get maximum of each category
      max_val = cat_val %>%
        max()
      
      # replace the category values with the mixed variable
      category_decomp[category_decomp$variable == cat, "contrib"] = cat_val - max_val
      
      # replace the base value with the base plus max value
      based_value = based_value + max_val
    }
    
    # replace the base value with the minned and maxed base value
    category_decomp[category_decomp$variable == "Base", "contrib"] = based_value
    
  }
  
  # return a list of category, variable tables, and fitted values
  l = list(
    category_decomp = category_decomp,
    variable_decomp = variable_decomp,
    fitted_values = fitted_values
  )
  
  return(l)
}

#' decomp_chart
#'
#' Variable Decomposition Bar Chart
#'
#' Plot the variable, or category, decomposition as stacked bars over the id variable which can be supplied to the \code{decomping} function.
#'
#' @param model Model object
#' @param decomp_list list object generated by the \code{decomping} function.
#' @param pool string specifying a group within the pool column to be filtered
#' @param colors character vector of colors in hexadecimal notation
#' @param variable_decomp boolean specifying whether the chart should be based on the variable_decomp or the category_decomp from the \code{decomping} function.
#' @param verbose A boolean to specify whether to print warnings
#' @param actual_line_color string representing chart color of the actual line
#' @param resid_line_color string representing chart color of residual line
#' @param plot_bgcolor string representing chart color of plot_bgcolor
#' @param paper_bgcolor string representing chart color of paper_bgcolor
#' @param font_color string representing chart color of font
#' @param grid_line_color string representing chart color of gridcolor
#' @param zero_line_color string representing chart color zerolinecolor
#' @import plotly
#' @import tidyverse
#' @export
#' @return a \code{plotly} bar chart of the model's decomposition
decomp_chart = function(model = NULL,
                        decomp_list = NULL,
                        pool = NULL,
                        colors = color_palette() %>% 
                          unlist() %>% 
                          as.character(),
                        variable_decomp = FALSE,
                        verbose = FALSE,
                        actual_line_color = 'black',
                        resid_line_color = 'red',
                        plot_bgcolor = "rgba(0, 0, 0, 0)",
                        paper_bgcolor = "rgba(0, 0, 0, 0)",
                        font_color =  '#1c0022',
                        zero_line_color = '#1c0022',
                        grid_line_color = '#1c0022') {
  
  # checks    ####
  
  # Check verbose
  if(!is.logical(verbose)){
    message("Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }

  # Check decomp_list , model
  if(is.null(model)){
    if(is.null(decomp_list)){
      message("Error: No decomp_list provided. Returning NULL. ")
      return(NULL)
    }
  }else{
    decomp_list = model$decomp_list
  }


  # get decomp
  if (variable_decomp) {
    # get variable decomp table
    decomp = decomp_list$variable_decomp
    title = 'Variable Decomposition'
  } else{
    # get category decomp table
    decomp = decomp_list$category_decomp
    title = 'Category Decomposition'
  }

  # check decomp table
  if(!is.data.frame(decomp)){
  }
  if(!all(c("pool","variable","contrib") %in% colnames(decomp))){
    message("Error: decomp table must include 3 columns called 'pool', 'variable' and 'value'. Returning NULL. ")
    return(NULL)
  }

  # get actual dependent variable table
  fitted_values = decomp_list$fitted_values

  # check fitted_values
  if(!is.data.frame(fitted_values)){
    ##
  }
  if(!all(c("pool","variable","value") %in% colnames(fitted_values))){
    message("Error: fitted_values table must include 3 columns called 'pool', 'variable' and 'value'. Returning NULL.")
    return(NULL)
  }

  fitted_values = fitted_values[fitted_values$variable %in% c("actual","predicted", "residual"), ]

  # the id variable name is the first column name
  id_var = colnames(decomp)[1]


  # filter by pool if provided
  if (!is.null(pool)) {

    if(!any(decomp$pool == pool)){
      message("Warning: POOL ",pool," not found. No POOL filtering applied.")
      decomp = decomp %>%
        rename(value = contrib)
    }
    else{
      decomp = decomp[decomp$pool == pool, ]%>%
        rename(value = contrib)
      fitted_values = fitted_values[fitted_values$pool == pool, ]
    }
  }

  if(is.null(pool)){
    if(verbose){
      message("Warning: No pool provided. Aggregating by id_var.")
    }

    fitted_values = fitted_values %>%
      group_by(variable,!!sym(id_var)) %>%
      summarise(value = sum(value)) %>%
      mutate(pool = "total_pool")

    decomp = decomp %>%
      group_by(variable,!!sym(id_var)) %>%
      summarise(contrib = sum(contrib)) %>%
      mutate(pool = "total_pool") %>%
      rename(value = contrib)

    pool = "total_pool"
  }

  # plot      ####
  
  # plot
  plot_ly(data = decomp,
          x = ~ get(id_var)) %>%
    add_trace(type = "bar",
              y = ~ value,
              color = ~ variable,
              name = ~ variable,
              colors = colors) %>%
    add_lines(
      data = fitted_values %>%
        filter(variable == "actual"),
      x = ~ get(id_var),
      y = ~ value,
      line = list(color = c(actual_line_color)),
      name = "actual"
    ) %>%
    add_lines(
      data = fitted_values %>%
        filter(variable == "residual"),
      x = ~ get(id_var),
      y = ~ value,
      line = list(color = c(resid_line_color)),
      name = ~ variable
    ) %>%
    layout(barmode = "relative",
           plot_bgcolor  = plot_bgcolor,
           paper_bgcolor = paper_bgcolor,
           title = title,
           margin = '10px',
           font = list(color = font_color),
           yaxis = list(gridcolor = grid_line_color),
           xaxis = list(title = id_var,
                        showgrid = FALSE,
                        zerolinecolor = zero_line_color))

}

#' total_decomp_chart
#'
#' Total Decomposition Stacked Chart
#'
#' Plot the total variable, or category, decomposition as a stacked bar chart.
#'
#' @param model Model object
#' @param decomp_list list object generated by the \code{decomping} function.
#' @param pool string specifying a group within the pool column to be filtered
#' @param colors character vector of colors in hexadecimal notation for bars
#' @param plot_bgcolor string representing chart color of plot_bgcolor
#' @param paper_bgcolor string representing chart color of paper_bgcolor
#' @param font_color string representing chart color of font
#' @param zero_line_color string representing chart color zerolinecolor
#' @param variable_decomp boolean specifying whether the chart should be based on the variable_decomp or the category_decomp from the \code{decomping} function.
#' @param verbose A boolean to specify whether to print warnings
#' @import plotly
#' @import tidyverse
#' @export
#' @return a \code{plotly} stacked bar chart of the model's decomposition
total_decomp_chart = function(model = NULL,
                        decomp_list = NULL,
                        pool = NULL,
                        colors = color_palette() %>% 
                          unlist() %>% 
                          as.character(),
                        variable_decomp = FALSE,
                        verbose = FALSE,
                        plot_bgcolor = "rgba(0, 0, 0, 0)",
                        paper_bgcolor = "rgba(0, 0, 0, 0)",
                        font_color =  '#1c0022',
                        zero_line_color = '#1c0022') {
  
  # checks    ####
  
  # Check verbose
  if(!is.logical(verbose)){
    message("Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  # Check decomp_list , model
  if(is.null(model)){
    if(is.null(decomp_list)){
      message("Error: No decomp_list provided. Returning NULL. ")
      return(NULL)
    }
  }else{
    decomp_list = model$decomp_list
  }
  
  
  # get decomp
  if (variable_decomp) {
    # get variable decomp table
    decomp = decomp_list$variable_decomp
    title = 'Variable Decomposition'
  } else{
    # get category decomp table
    decomp = decomp_list$category_decomp
    title = 'Category Decomposition'
  }
  
  # check decomp table
  if(!is.data.frame(decomp)){
  }
  if(!all(c("pool","variable","contrib") %in% colnames(decomp))){
    message("Error: decomp table must include 3 columns called 'pool', 'variable' and 'value'. Returning NULL. ")
    return(NULL)
  }
  
  # the id variable name is the first column name
  id_var = colnames(decomp)[1]
  
  
  # filter by pool if provided
  if (!is.null(pool)) {
    
    if(!any(decomp$pool == pool)){
      message("Warning: POOL ",pool," not found. No POOL filtering applied.")
      decomp = decomp %>%
        rename(value = contrib)
    }
    else{
      decomp = decomp[decomp$pool == pool, ]%>%
        rename(value = contrib)
    }
  }
  
  if(is.null(pool)){
    if(verbose){
      message("Warning: No pool provided. Aggregating by id_var.")
    }
    
    decomp = decomp %>%
      group_by(variable,!!sym(id_var)) %>%
      summarise(contrib = sum(contrib)) %>%
      mutate(pool = "total_pool") %>%
      rename(value = contrib)
    
    pool = "total_pool"
  }
  
  # plot      ####
  
  # plot
  plot_ly(data = decomp %>% 
            group_by(variable) %>% 
            summarise(value = sum(value))%>%
            arrange(-value)) %>% 
    add_bars(
    x = ~ value,
    color = ~ variable,
    colors = colors,
    y = "total"
  ) %>%
    layout(
      margin = '10px',
      barmode = "relative",
      plot_bgcolor  = plot_bgcolor,
      paper_bgcolor = paper_bgcolor,
      font = list(color = font_color),
      xaxis = list(showgrid = F, zerolinecolor = zero_line_color),
      yaxis = list(showgrid = F),
      title = 'Total Decomposition'
    ) 
  
}


#' fit_chart
#'
#' Dependent Variable, Predictions and Residuals Line Chart
#'
#' Plot the dependent variable, predictions and Residuals as a line chart over the id variable which can be supplied to the \code{decomping} function.
#'
#' @param model Model object
#' @param decomp_list list object generated by the \code{decomping} function.
#' @param pool string specifying a group within the pool column to be filtered
#' @param colors character vector of colors in hexadecimal notation
#' @param predicted_line_color string representing chart color of the predicted line
#' @param actual_line_color string representing chart color of the actual line
#' @param resid_line_color string representing chart color of residual line
#' @param plot_bgcolor string representing chart color of plot_bgcolor
#' @param paper_bgcolor string representing chart color of paper_bgcolor
#' @param font_color string representing chart color of font
#' @param grid_line_color string representing chart color of gridcolor
#' @param zero_line_color string representing chart color zerolinecolor
#' @param verbose A boolean to specify whether to print warnings
#' @import plotly
#' @import tidyverse
#' @export
#' @return a \code{plotly} line chart of the model's prediction and actual
#' @examples
#' run_model(data = mtcars,dv = 'mpg',ivs = 'cyl') %>% fit_chart()
fit_chart = function(model = NULL,
                     decomp_list = NULL,
                     pool = NULL,
                     verbose = FALSE,
                     colors = NULL,
                     predicted_line_color =  "#00cf74",
                     actual_line_color = "#1c0022",
                     resid_line_color = '#c90f3a',
                     plot_bgcolor = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     font_color =  '#1c0022',
                     zero_line_color = '#1c0022',
                     grid_line_color = '#1c0022') {
  # test    ####
  
  # data = sales_ts
  # dv = "sales"
  # ivs = c("dec", "mar")
  # id_var = "week"
  # model = run_model(
  #   verbose = FALSE,
  #   data = data,
  #   dv = dv,
  #   ivs = ivs,
  #   normalise_by_pool = FALSE,
  #   id_var = id_var 
  # )
  # model %>% fit_chart()
  # verbose = TRUE
  # colors = NULL
  # pool = NULL
  
  # checks  #####
  
  # Check verbose
  if(!is.logical(verbose)){
    message("Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  if(verbose){
    message("Fit chart...")
  }


  # Check decomp_list , model
  if(is.null(model)){
    if(is.null(decomp_list)){
      message("- Error: No decomp_list nor model provided. Returning NULL.")
      return(NULL)
    }
  }else{
    decomp_list = model$decomp_list
  }
  # get actual dependent variable table
  fitted_values = decomp_list$fitted_values

  # the id variable name is the first column name
  id_var = colnames(fitted_values)[1]

  # filter by pool if provided
  if (!is.null(pool)) {
    if(!any(fitted_values$pool == pool)){
      message("- Warning: POOL ",pool," not found. No POOL filtering applied.")
    }
    else{
      fitted_values = fitted_values[fitted_values$pool == pool, ]
    }
  }

  if(is.null(pool)){
    if(verbose){
      message("- Warning: No pool provided. Aggregating by id_var.")
    }
    fitted_values = fitted_values %>%
      group_by(variable,!!sym(id_var)) %>%
      summarise(value = sum(value)) %>%
      mutate(pool = "total_pool")

    pool = "total_pool"
  }

  # plot    ####
  

  plot_ly(fitted_values) %>%
    add_lines(
      data = filter(fitted_values, variable == "residual"),
      x = ~get(id_var),
      y = ~ value,
      line = list(color = resid_line_color),
      color =  ~ variable
    ) %>%
    add_lines(
      data = filter(fitted_values, variable == "actual"),
      x = ~ get(id_var),
      y = ~ value,
      line = list(color = actual_line_color),
      color =  ~ variable
    ) %>%
    add_lines(
      data = filter(fitted_values, variable == "predicted"),
      x = ~ get(id_var),
      y = ~ value,
      line = list(color = predicted_line_color),
      color =  ~ variable
    ) %>%
    layout(
      margin = '10px',
      plot_bgcolor  = plot_bgcolor,
      paper_bgcolor = paper_bgcolor,
      title = 'Fit Chart',
      font = list(color = font_color),
      yaxis = list(gridcolor = grid_line_color),
      xaxis = list(
        showgrid = FALSE,
        zerolinecolor = zero_line_color,
        title = id_var
      )
    )

}

#' add_total_pool
#'
#' Add an aggregated decomposition.
#'
#' When running a pooled model, it might be desirable to view the output of the `decomping()` function as an aggregate of all pools.
#'
#' @param model Model object
#' @param decomp_list list object generated by the \code{decomping} function.
#' @param verbose A boolean to specify whether to print warnings
#' @import dplyr
#' @export
#' @return a \code{list} of 3 \code{data.frame}'s representing the variable and category decomposition, and the fitted values.
add_total_pool = function(
    model = NULL,
    decomp_list = NULL,
    verbose = FALSE){
  # checks ------------------
  
  # check verbose
  if(!is.logical(verbose)){
    message("Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }
  
  # check inputs
  if(is.null(model) & is.null(decomp_list)){
    message('Error: Neither "model" nor "decomp_list" were provided. Returning NULL')
    return(NULL)
  }
  if(!is.null(model)){
    if(!is.null(decomp_list)){
      if(verbose)message('Warning: Both "model" and "decomp_list" have been provided. "model" will be used and "decomp_list" will be ignored.')
    }
    
    if(is.null(model$decomp_list)){
      if(!is.null(decomp_list)){
        if(verbose)message('Warning: "model" does not contain "decomp_list". Using "decomp_list" provided.')
      }
    }else{
      decomp_list = model$decomp_list
    }
  }

  # process -----------------------------------------------------------------
  
  variable_decomp = decomp_list$variable_decomp
  category_decomp = decomp_list$category_decomp
  fitted_values = decomp_list$fitted_values
  
  id = colnames(variable_decomp)[1]
  
  variable_decomp = variable_decomp %>%
    bind_rows(
      variable_decomp %>%
        group_by(!!sym(id),variable) %>%
        summarise(contrib = sum(contrib)) %>%
        mutate(pool = 'Total')
    )
  
  category_decomp = category_decomp %>%
    bind_rows(
      category_decomp %>%
        group_by(!!sym(id),variable) %>%
        summarise(contrib = sum(contrib)) %>%
        mutate(pool = 'Total')
    )
  
  fitted_values = fitted_values %>%
    bind_rows(
      fitted_values %>%
        group_by(!!sym(id),variable) %>%
        summarise(value = sum(value)) %>%
        mutate(pool = 'Total')
    )
  
  decomp_list$variable_decomp = variable_decomp
  decomp_list$category_decomp = category_decomp
  decomp_list$fitted_values = fitted_values
  
  return(decomp_list)
  
}

#' add_total_pool_to_data
#'
#' Add an aggregated set of observations to a \code{data.frame}
#'
#' Add an aggregated set of observations to a \code{data.frame} based on a "pool" variable provided.
#'
#' @param data A \code{data.frame} containing the pool and id variables provided.
#' @param pool_var A string representing the variable name of the pool variable.
#' @param id_var A string representing the variable name of the id variable.
#' @import dplyr
#' @export
#' @return a \code{data.frame} with additional observations.
add_total_pool_to_data = function(data,pool_var,id_var) {
  
  # calculate the aggregated pools
  totals = data %>%
    group_by(!!sym(id_var)) %>%
    select_if(is.numeric) %>%
    summarise_all(.funs = sum) %>%
    mutate(pool_var = 'Total')
  
  # rename the columns of the aggregated df to match the original
  cols = colnames(totals)
  cols[cols == 'pool_var'] = pool_var
  colnames(totals) = cols
  
  # append the aggregated rows to the original data
  data = data %>%
    bind_rows(totals)
  
  return(data)
  
}




#' filter_decomp_pool
#'
#' Filter a model's decomposition based on a given pool.
#'
#' Filter all \code{data.frame}'s within a model's decomposition based on a given pool.
#'
#' @param decomp A \code{list} of \code{data.frame} from a model object or generated using \code{decomping}.
#' @param pool A string representing the variable name of the pool variable.
#' @param verbose A boolean to specify whether to print warnings
#' @import dplyr
#' @export
#' @return a \code{list} of 3 \code{data.frame}'s representing the variable and category decomposition, and the fitted values.
filter_decomp_pool = function(decomp,pool,verbose = TRUE){
  # if (!(pool %in% colnames(decomp$variable_decomp))) {
  #   if(verbose){
  #     message("Error: pool string provided does not match decomp columns.")
  #   }
  #   return(decomp)
  # }
  
  variable_decomp = decomp$variable_decomp %>%
    filter(pool == !!pool)
  category_decomp = decomp$category_decomp%>%
    filter(pool == !!pool)
  fitted_values = decomp$fitted_values%>%
    filter(pool == !!pool)
  
  decomp$variable_decomp = variable_decomp
  decomp$category_decomp = category_decomp
  decomp$fitted_values = fitted_values
  
  return(decomp)
  
  
}



#' resid_hist_chart
#'
#' Histogram of Model Residuals
#'
#' Plot a histogram to visualise the distribution of residuals.
#' This is meant to assess the residual distribution's normality.
#'
#' @param model Model object
#' @param decomp_list list object generated by the \code{decomping} function.
#' @param pool string specifying a group within the pool column to be filtered
#' @param color string specifying bar color
#' @param plot_bgcolor string representing chart color of plot_bgcolor
#' @param paper_bgcolor string representing chart color of paper_bgcolor
#' @param font_color string representing chart color of font
#' @param grid_line_color string representing chart color of gridcolor
#' @param zero_line_color string representing chart color zerolinecolor
#' @param verbose A boolean to specify whether to print warnings
#' @import plotly
#' @import tidyverse
#' @export
#' @return a \code{plotly} histogram of the model's residuals
resid_hist_chart = function(model = NULL,
                            decomp_list = NULL,
                            pool = NULL,
                            color = "black",
                            plot_bgcolor = "rgba(0, 0, 0, 0)",
                            paper_bgcolor = "rgba(0, 0, 0, 0)",
                            font_color =  '#1c0022',
                            zero_line_color = '#1c0022',
                            grid_line_color = '#1c0022',
                            verbose = FALSE){
  # Check verbose
  if(!is.logical(verbose)){
    message("Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }


  # Check decomp_list , model
  if(is.null(model)){
    if(is.null(decomp_list)){
      if(verbose){
        message("Error: No decomp_list provided. Returning NULL.")
      }
      return(NULL)
    }
  }else{
    decomp_list = model$decomp_list
  }

  df = decomp_list$fitted_values %>%
    filter(variable == "residual") %>%
    tibble() %>%
    select(value) %>%
    rename(residual = 1)

  if(!is.null(pool)){
    pool_var = decomp_list$fitted_values %>%
      filter(variable == "residual") %>%
      pull(pool)

    if(pool %in% pool_var){
      df = df[pool_var==pool,]
    }
  }

  plot_ly(data = df) %>%
    add_histogram(histnorm = "probability",
                  x = ~residual,
                  marker = list(color = color,
                                line = list(color = "white",
                                            width = .5))) %>%
    layout(font = list(color = font_color),
           margin = '10px',
           title = 'Residual Distribution',
           yaxis = list(gridcolor = grid_line_color),
           plot_bgcolor  = plot_bgcolor,
           paper_bgcolor = paper_bgcolor)

}

#' heteroscedasticity_chart
#'
#' Scatter of Residuals over dependent Variable
#'
#' Plot a scatter chart of residuals over the dependent variable.
#' This is meant to assess the consistency of the residuals' variance across the dependent variable.
#'
#' @param model Model object
#' @param decomp_list list object generated by the \code{decomping} function.
#' @param pool string specifying a group within the pool column to be filtered
#' @param color string specifying bar color
#' @param plot_bgcolor string representing chart color of plot_bgcolor
#' @param paper_bgcolor string representing chart color of paper_bgcolor
#' @param font_color string representing chart color of font
#' @param grid_line_color string representing chart color of gridcolor
#' @param zero_line_color string representing chart color zerolinecolor
#' @param verbose A boolean to specify whether to print warnings
#' @import plotly
#' @import tidyverse
#' @importFrom tidyr pivot_wider
#' @export
#' @return a \code{plotly} scatter chart of the model's dependent variable over residuals
heteroskedasticity_chart = function(model = NULL,
                                    decomp_list = NULL,
                                    pool = NULL,
                                    color = "black",
                                    plot_bgcolor = "rgba(0, 0, 0, 0)",
                                    paper_bgcolor = "rgba(0, 0, 0, 0)",
                                    font_color =  '#1c0022',
                                    zero_line_color = '#1c0022',
                                    grid_line_color = '#1c0022',
                                    verbose = FALSE){

  # Check verbose
  if (!is.logical(verbose)) {
    message("Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }


  # Check decomp_list , model
  if (is.null(model)) {
    if (is.null(decomp_list)) {
      if (verbose) {
        message("Error: No decomp_list provided. Returning NULL.")
      }
      return(NULL)
    }
  } else{
    decomp_list = model$decomp_list
  }


  df = decomp_list$fitted_values %>%
    filter(variable != 'prediction') %>%
    pivot_wider(names_from  = variable)

  if (!is.null(pool)) {
    pool_var = df %>%
      pull(pool)

    if (pool %in% pool_var) {
      df = df[pool_var == pool, ]
    } else{
      message('Warning: pool not found in data. Using full data.')
    }
  }

  plot_ly(data = df) %>%
    add_trace(
      x = ~ residual,
      y = ~ actual,
      type = 'scatter',
      mode = 'markers',
      marker = list(color = color,
                    line = list(color = "white",
                                width = .5))
    ) %>%
    layout(
      font = list(color = font_color),
      margin = '10px',
      title = 'Heteroskedasticity',
      plot_bgcolor  = plot_bgcolor,
      paper_bgcolor = paper_bgcolor,
      yaxis = list(gridcolor = grid_line_color),
      xaxis = list(showgrid = FALSE,
                   zerolinecolor = zero_line_color)
    )

}


#' acf_chart
#'
#' Bar chart of autocorrelation function
#'
#' A bar chart meant to assess the correlation of the residuals with lagged versions of themselves.
#'
#' @param model Model object
#' @param decomp_list list object generated by the \code{decomping} function.
#' @param pool string specifying a group within the pool column to be filtered
#' @param color string specifying bar color
#' @param plot_bgcolor string representing chart color of plot_bgcolor
#' @param paper_bgcolor string representing chart color of paper_bgcolor
#' @param font_color string representing chart color of font
#' @param grid_line_color string representing chart color of gridcolor
#' @param zero_line_color string representing chart color zerolinecolor
#' @param verbose A boolean to specify whether to print warnings
#' @import plotly
#' @import tidyverse
#' @export
#' @return a \code{plotly} bar chart of the model's ACF
acf_chart = function(model = NULL,
                     decomp_list,
                     pool = NULL,
                     color = "black",
                     plot_bgcolor = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)",
                     font_color =  '#1c0022',
                     zero_line_color = '#1c0022',
                     grid_line_color = '#1c0022',
                     verbose = FALSE){

  # Check verbose
  if (!is.logical(verbose)) {
    message("Warning: verbose must be logical (TRUE or FALSE). Setting to TRUE.")
    verbose = TRUE
  }


  # Check decomp_list , model
  if (is.null(model)) {
    if (is.null(decomp_list)) {
      if (verbose) {
        message("Error: No decomp_list provided. Returning NULL.")
      }
      return(NULL)
    }
  } else{
    decomp_list = model$decomp_list
  }



  df = decomp_list$fitted_values %>%
    filter(variable == "residual") %>%
    tibble() %>%
    select(value) %>%
    rename(residual = 1)

  if(!is.null(pool)){
    pool_var = decomp_list$fitted_values %>%
      filter(variable == "residual") %>%
      pull(pool)

    if(pool %in% pool_var){
      df = df[pool_var==pool,]
    }
  }

  x = acf(df$residual, plot = FALSE)
  x = data.frame(x$acf) %>%
    rownames_to_column("x") %>%
    mutate(x = as.numeric(x))


  plot_ly() %>%
    add_trace(y = x$x.acf, x = x$x,marker = list(color = color,
                                                 line = list(color = "white",
                                                             width = .5)),type="bar") %>%
    add_trace(
      showlegend = FALSE,
      y = rep(0.2, length(x$x)),
      x = x$x,
      type = 'scatter',
      mode = 'lines',
      hoverinfo = 'skip',
      line = list(color = "rgba(0, 0, 0, 0.5)",dash = 'dot')
    ) %>%
    add_trace(
      hoverinfo = 'skip',
      showlegend = FALSE,
      y = rep(-0.2, length(x$x)),
      x = x$x,
      type = 'scatter',
      mode = 'lines',
      line = list(color = "rgba(0, 0, 0, 0.5)",dash = 'dot')
    ) %>%
    add_trace(
      hoverinfo = 'skip',
      showlegend = FALSE,
      y = rep(0.4, length(x$x)),
      x = x$x,
      type = 'scatter',
      mode = 'lines',
      line = list(color = "rgba(0, 0, 0, 0.75)",dash = 'dot')
    ) %>%
    add_trace(
      hoverinfo = 'skip',
      showlegend = FALSE,
      y = rep(-0.4, length(x$x)),
      x = x$x,
      type = 'scatter',
      mode = 'lines',
      line = list(color = "rgba(0, 0, 0, 0.75)",dash = 'dot')
    ) %>%
    layout(
      font = list(color = font_color),
      margin = '10px',
      title = 'Autocorrelation Function',
      plot_bgcolor  = plot_bgcolor,
      paper_bgcolor = paper_bgcolor,
      yaxis = list(gridcolor = grid_line_color,
                   title = 'correlation'),
      xaxis = list(
        title = 'lags',
        showgrid = FALSE,
        zerolinecolor = zero_line_color)
    )

}

#' response_curves
#'
#' Line chart of variable response curves
#'
#' Line chart of variable response curves visualising the relationship of each independent variable with the dependent variable
#'
#' @param model Model object
#' @param x_min number specifying horizontal axis min
#' @param x_max number specifying horizontal axis max
#' @param y_min number specifying vertical axis min
#' @param y_max number specifying vertical axis max
#' @param interval number specifying interval between points of the curve
#' @param pool string specifying a group within the pool column to be filtered
#' @param trans_only a boolean specifying whether to display non-linear only \code{y = b*dim_rest(x)}
#' @param colors character vector of colors in hexadecimal notation
#' @param plot_bgcolor string representing chart color of plot_bgcolor
#' @param paper_bgcolor string representing chart color of paper_bgcolor
#' @param font_color string representing chart color of font
#' @param grid_line_color string representing chart color of gridcolor
#' @param zero_line_color string representing chart color zerolinecolor
#' @param verbose A boolean to specify whether to print warnings
#' @param table A boolean to specify whether to return a \code{data.frame} of the response curves
#' @param points A boolean to specify whether to include the points from the data on the curve
#' @param plotly A boolean to specify whether to include use ggplot over plotly
#' @param add_intercept A boolean to specify whether to include the intercept whne calculating the curves
#' @importFrom ggplot2 ggplot geom_line scale_color_manual theme ggtitle ylab geom_vline geom_hline element_rect aes
#' @import plotly
#' @import tidyverse
#' @import tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats na.omit
#' @export
#' @return a \code{plotly} line chart of the model's response curves
#' @examples
#' model = run_model(data = mtcars,
#'                   dv = 'mpg',
#'                   ivs = c('disp','wt'),
#'                   pool_var='cyl',
#'                   normalise_by_pool=TRUE)
#' model %>%
#'    response_curves(pool='4')
#' model = run_model(data = mtcars,dv = 'mpg',ivs = c('wt','cyl','disp')) 
#' 
#' model %>%
#'    response_curves()
#'    
#' run_model(data = scale(mtcars) %>% 
#'               data.frame(),
#'           dv = 'mpg',
#'           ivs = c('wt','cyl','disp')) %>%
#'    response_curves(points = TRUE,x_min = -3,x_max = 3)
response_curves = function(
    model,
    x_min = NULL,
    x_max = NULL,
    y_min = NULL,
    y_max = NULL,
    interval = NULL,
    pool = NULL,
    trans_only = FALSE,
    colors = color_palette() %>% 
      unlist() %>% 
      as.character(),
    plot_bgcolor = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    font_color =  '#1c0022',
    zero_line_color = '#1c0022',
    grid_line_color = '#1c0022',
    plotly = TRUE,
    allow_ts_trans = FALSE,
    verbose = FALSE,
    table = FALSE,
    histogram = FALSE,
    add_intercept = FALSE,
    points = FALSE){
  
  # test    #####
  
  # # non-ts model
  # data = mtcars %>%
  #   rownames_to_column('cars')
  # dv = 'mpg'
  # id_var = 'cars'
  # ivs = c('disp','wt','qsec')
  # pool_var='cyl'
  # model_table = build_model_table(ivs)
  # model_table = model_table %>%
  #   mutate(hill = if_else(variable=='qsec','20,5',hill))
  # 
  # model = run_model(data = data,
  #                   id_var = id_var,
  #                   dv = dv,
  #                   model_table = model_table
  #                   # ivs = ivs,
  #                   # pool_var= pool_var,
  #                   # normalise_by_pool = T
  #                   )
  # 
  # model %>% response_curves(
  #   points = T,
  #   trans_only = T,
  #   x_max = 150,
  #   x_min = 0,
  #   histogram = T)

  # x_min = 0
  # x_max = 30
  # y_min = NULL
  # y_max = NULL
  # interval = NULL
  # trans_only = T
  # colors = color_palette()
  # plotly = TRUE
  # verbose = TRUE
  # allow_ts_trans = FALSE
  # table = FALSE
  # pool = NULL
  # pool = '6'
  # add_intercept = FALSE
  # points = FALSE
  # histogram = TRUE
  # plot_bgcolor = "rgba(0, 0, 0, 0)"
  # paper_bgcolor = "rgba(0, 0, 0, 0)"
  # font_color =  '#1c0022'
  # zero_line_color = '#1c0022'
  # grid_line_color = '#1c0022'

  # checks  ####
  
  # Check verbose
  if (!is.logical(verbose)) {
    message("Warning: verbose must be logical (TRUE or FALSE). Setting to TRUE.")
    verbose = TRUE
  }
  
  if(verbose){
    message("Generating response curves...")
  }  
  
  output_model_table = model$output_model_table
  
  
  if (is.null(x_max)) x_max = 1e5
  if (is.null(x_min)) x_min = -1e5
    
  # if (is.null(x_max)){ 
  #   
  #   if(!is.null(model$raw_data)){
  #     
  #     x_max = model$raw_data %>%
  #       select(all_of(output_model_table$variable[output_model_table$variable != "(Intercept)"])) %>% 
  #       max()
  #     
  #   }else{x_max = 1e+05}
  #   
  # }
  # if (is.null(x_min)){ 
  #   
  #   if(!is.null(model$raw_data)){
  #     
  #     x_min = model$raw_data %>%
  #       select(all_of(output_model_table$variable[output_model_table$variable != "(Intercept)"])) %>% 
  #       min()
  #     
  #   }else{x_min = -1e+05}
  #   
  # }
  if (is.null(interval)) interval = (x_max-x_min)/100
  if (is.null(y_max)) y_max = x_max
  if (is.null(y_min)) y_min = x_min

  # process ####
  
  if(!is.null(pool) & !is.null(model$pool_mean)){
    mean = model$pool_mean$mean[model$pool_mean$pool==pool]
    if(!(pool %in% model$pool_mean$pool)){
      if(verbose){message('- Warning: pool provided not found in model pools.')}
    }else{
      output_model_table =  output_model_table %>% 
        mutate(coef = coef * mean)
    }
  }
  if(!is.null(pool) & is.null(model$pool_mean)){
    if(verbose){message('- Info: model normalised by pool but no pool provided.')}
  }
  if(is.null(pool) & !is.null(model$pool_mean)){
    if(verbose){message('- Warning: pool provided but model is not normalised by pool.')}
  }

  trans_df = model$trans_df
  if(!allow_ts_trans){
    trans_df = trans_df%>%
      filter(ts == FALSE) 
  }

  if (trans_only) {
    output_model_table = output_model_table[!(((
      output_model_table[trans_df$name] == "") %>%
        data.frame() %>% 
        rowSums()) == nrow(trans_df)),
    ]
  }
  output_model_table = output_model_table %>% 
    filter(variable != "(Intercept)") %>% 
    select(all_of(c("variable", "variable_t", trans_df$name, "coef"))) %>%
    na.omit()
  
  if (nrow(output_model_table) == 0) {
    message("- Error: Check model, model_table, and/or trans_df as well as other input arguments for the `response_curves()` function.")
    return(NULL)
  }
  curves_df = list()
  x_raw = seq(x_min, x_max, interval)
  coefs = output_model_table$coef
  
  for (i in 1:nrow(output_model_table)) {
    var = output_model_table$variable_t[i]
    coef = coefs[i]
    x = x_raw
    for (j in 1:nrow(trans_df)) {
      t_name = trans_df$name[j]
      t_func = trans_df$func[j]
      param_vals = model$output_model_table %>%
        filter(variable_t == var) %>%
        pull(!!sym(t_name)) %>%
        strsplit(split = ",")

      param_vals = param_vals[[1]] %>% as.numeric()
      if (length(param_vals) == 0) {
        next
      }
      param_names = letters[1:length(param_vals)]
      e <- new.env()
      for (k in 1:length(param_vals)) {
        p_name = param_names[k]
        p_val = param_vals[k]
        assign(p_name, p_val, envir = e)
      }
      x = t_func %>% run_text(env = e)
    }
    
    df = data.frame(value = x * coef,
                    variable = var,
                    x = x_raw) %>%
      mutate(value = as.numeric(value)) %>%
      mutate(variable = as.character(variable)) %>%
      mutate(x = as.numeric(x))
    
    curves_df = append(curves_df, list(df))
  }
  curves_df = curves_df %>%
    Reduce(f = rbind)

  if(add_intercept){
    curves_df = curves_df %>%
      mutate(value = value + model$coefficients[1])
  }

  curves_df = curves_df %>%
    filter(value >= y_min) %>%
    filter(value <= y_max)


  if (table) {
    if(verbose)message("Returning response curves table.")
    return(curves_df)
  }

  # plotly  ####
  if(plotly){

    p = plot_ly()
    p = p %>% add_trace(data = curves_df, 
                        x = ~x, 
                        y = ~value,
                        color = ~variable, 
                        legendgroup = ~variable,
                        mode = "lines", 
                        type = "scatter",
                        colors = colors) %>%
      layout(plot_bgcolor = plot_bgcolor,
             margin = '10px',
             paper_bgcolor = paper_bgcolor,
             font = list(color = font_color),
             xaxis = list(showgrid = FALSE,
                          zerolinecolor = zero_line_color),
             yaxis = list(gridcolor = grid_line_color,
                          title = model$dv),
             title = "Response Curves")

    if(points | histogram) {

      id_var = model$id_var
      
      if(histogram){
        
        raw_data = model$raw_data %>%
          select(all_of(c(id_var,output_model_table$variable))) 
        
        hist_df = lapply(output_model_table$variable, function(var){
          
          h = hist(raw_data %>% pull(!!sym(var)), plot=FALSE)
          
          h = data.frame(
            x = linea::ma(h$breaks,2,align = "center")[-length(h$breaks)],
            y = h$counts,
            var = var,
            var_t = output_model_table$variable_t[output_model_table$variable == var]
            )
          
          return(h)
        }) %>% 
          Reduce(f = rbind)
      
        p = p %>%  add_bars(
          alpha = 0.6,
          data = hist_df ,
          x = ~ x,
          y = ~ y,
          yaxis = "y2",
          color = ~ var_t,
          legendgroup = ~var_t,
          showlegend = FALSE,
          colors = colors
        ) 
        
        p = p %>%
          layout(
            barmode = "overlay",
            yaxis2 = list(
            overlaying = "y",
            side = "right",
            title = "frequency"
            
          ))
      }
      
      if(points){
        
        variable_decomp = model$decomp_list$variable_decomp
        melted_raw_data = raw_data %>% 
          reshape2::melt(id.vars = id_var) %>% 
          left_join(
            output_model_table %>% 
              select(variable,variable_t),
            by = "variable"
          )
        
        
        points_df = variable_decomp %>% 
          filter(variable != "(Intercept)") %>% 
          left_join(melted_raw_data,by = c("variable" = "variable_t",id_var))
        
        p = p %>%  
          add_trace(
            alpha = 0.4,
            data = points_df, 
            x = ~value, 
            y = ~contrib,
            color = ~variable, 
            legendgroup = ~variable,
            showlegend = FALSE,
            mode = "markers",
            marker = list(size = 7),
            type = "scatter",
            colors = colors)
      }


    }
    
  }
  # ggplot  ####
  if(!plotly){
    
    p = ggplot(data = curves_df, aes(x = x, y = value, col = variable)) +
      geom_line() +
      scale_color_manual(values = color_palette() %>% 
                           unlist() %>% 
                           as.character()) +
      theme(
        panel.background = element_rect(fill = "white",
                                        colour = "white")) +
      ggtitle("Response Curves") +
      ylab(model$dv) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0)
    
  }

  
  if(verbose)message("Returning response curves plot.")
  return(p)
}
