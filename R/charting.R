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
    c(
      "#636363",
      "#143fba",
      "#006a53",
      "#209162",
      "#33eea0",
      "#8f9d35",
      "#fff200",
      "#ff5c00",
      "#6a000d",
      "#ff006e",
      "#b400da",
      "#280030"
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
#' @param raw_data \code{data.frame} containing data for analysis
#' @param categories \code{data.frame} mapping variables to groups
#' @param id_var string of id variable name (e.g. date)
#' @param verbose A boolean to specify whether to print warnings
#' @import tidyverse
#' @importFrom stats complete.cases
#' @importFrom methods is
#' @importFrom magrittr '%>%'
#' @importFrom reshape2 melt acast
#' @return a \code{list} of 3 \code{data.frame}'s representing the variable and category decomposition, and the fitted values.
#' @examples
#' run_model(data = mtcars,dv = 'mpg',ivs = c('wt','cyl','disp')) %>% decomping()
decomping = function(model = NULL,
                     de_normalise = TRUE,
                     raw_data = NULL,
                     categories = NULL,
                     id_var = NULL,
                     verbose = FALSE){
  # checks ####

  # check verbose
  if(!is.logical(verbose)){
    cat("\n Warning: verbose provided mus be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }


  # check if model is provided
  if(is.null(model)){
    if(verbose){
      cat("\n Error: no model provided. Returning NULL.")
    }
    return(NULL)
  }
  if(!is(model,'lm')){
    if(verbose){
      cat("\n Error: model must be of type 'lm'. Returning NULL.")
    }
    return(NULL)
  }


  # get the coefficients from the model object
  coef = model$coefficients
  ivs = names(coef)[-1]

  if(!("model_ table" %in% names(model))){
    model_table = build_model_table(ivs = ivs)
  }


  # extract dependent variable name
  dv = model$dv

  # get the modeled data from the model object
  data = model$model

  # get the dependent variable from the data object
  actual = data[, dv]

  # initiate variable to confirm if correct raw data is provided
  raw_actual_supplied = FALSE

  # process ####

  # in raw data is supplied check for and drop NAs
  if(!is.null(raw_data)){

    #check raw_data
    if(!is.data.frame(raw_data)){
      if (verbose) {
        cat("\n Warning: raw_data must be a data.frame.")
      }
    }else if(any(complete.cases(raw_data))) {
      if(verbose){
        cat("\n Warning: NA's found in raw data will be dropped.")
      }
      raw_data = raw_data[complete.cases(raw_data), ]
    }
  }


  if(!is.logical(de_normalise)){
    if(verbose){
      cat("\n Warning: de_normalise provided must be of type logical. Setting de_normalise to FALSE.")
    }
    de_normalise = FALSE
  }

  # get raw dependent variable if supplied
  if (de_normalise) {
    # if no raw data provided
    if (is.null(raw_data) | !is.data.frame(raw_data)) {
      if(verbose)cat("\n Warning: you must provide a raw_data data.frame to de normalise the data.")
    }

    # if the raw data is supplied
    else{
      # try to get the dependent variable from the raw data
      raw_actual = TRY({
        raw_data %>%
          select(!!sym(dv))
      })

      # if the dependent variable is found in the raw data provided
      if (!is.null(raw_actual)) {
        # switch raw_actual_supplied variable to TRUE
        raw_actual_supplied = TRUE

      } else{
        # else if(verbose)print warning
        if(verbose)cat("\n Warning: dependent variable not found in raw_data supplied.")

      }
    }
  }


  # get the intercept value from the coef object
  intercept = coef[1]
  # keep the other coefficients
  coef = coef[2:length(coef)]

  # generate an id variable if one is not provided
  if (is.null(id_var)) {
    if(verbose)cat(paste0(
      "\n Info: no id variable supplied. New id variable generated as 1 to ",
      nrow(data)
    ))
    id_var = "id"
    id_var_values = 1:nrow(data)
  } else{
    # if and id_var is provided, check that raw data is provided
    if (is.null(raw_data)) {
      # if raw data not provided if(verbose)print warning and generate id_var_values
      if(verbose)cat(
        paste0(
          "\n Warning: ID variable provided, but no raw data provided. New id variable generated as 1 to ",
          nrow(data)
        )
      )
      id_var_values = 1:nrow(data)
    }
    else{
      # if raw data is provided, check that raw data contains the id variable
      if (id_var %in% colnames(raw_data)) {
        id_var_values = raw_data %>% pull(!!sym(id_var))
      } else{
        # if raw data doesnt contain the id_var, if(verbose)print warning and generate id_variable
        if(verbose)cat(
          paste0(
            "\n Warning: ID variable provided not found in raw data provided. New id variable generated as 1 to ",
            nrow(data)
          )
        )
        id_var_values = 1:nrow(data)
      }
    }
  }

  # try to get the normalisation table
  meta_data = model$meta_data

  # if no meta_data is provided
  if (is.null(meta_data)) {
    if(verbose){
      cat(
        "\n Info: no normalisation table (meta_data) found in model object. A pool variable ('total') will be generated."
      )}

    pool = tibble("total_pool")
  } else{
    # if a norm table is provided extract the pool variable
    pool_variable = meta_data$variable[toupper(meta_data$meta) == "POOL"]

    # if no raw data provided if(verbose)print a warning and generate a pool variable
    if (is.null(raw_data)) {
      if(verbose)cat(
        "\n Warning: no raw_data found to extract pool variable found in model's meta_data. A pool variable ('total') will be generated."
      )

      pool = tibble("total_pool")
    } else if (length(pool_variable) > 0) {
      # if raw data is provided check if it contains the pool variable
      if (pool_variable %in% colnames(raw_data)) {
        pool = raw_data[, pool_variable]
      } else{
        # if not, if(verbose)print warning and geterate pool variable
        if(verbose)cat(
          "\n Warning: pool variable from model's meta_data not found in raw_data. A pool variable ('total_pool') will be generated."
        )
        pool = tibble("total_pool")
      }
    }else{
      # if not, if(verbose)print warning and geterate pool variable
      if(verbose)cat(
        "\n Warning: pool variable from model's meta_data not found in raw_data. A pool variable ('total_pool') will be generated."
      )
      pool = tibble("total_pool")
    }
  }

  # generate the fitted values dataframe
  fitted_values = tibble(
    actual = c(actual),
    residual = model$residuals,
    predicted = model$fitted.values,
    id = id_var_values,
    pool = pool %>% pull()
  ) %>%
    reshape2::melt(id.vars = c("id", "pool"),factorsAsStrings=FALSE)

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

  # generate tibble df using the variable decomp, intercept and id variable
  variable_decomp = tibble(
    "(Intercept)" = intercept,
    variable_decomp,
    id = id_var_values,
    pool = pool %>% pull()
  ) %>%
    reshape2::melt(id.vars = c("id", "pool"),factorsAsStrings=FALSE) %>%
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
  if (raw_actual_supplied & de_normalise) {
    # check if meta_data is provided
    if (is.null(meta_data)) {
      if(verbose)cat("\n Warning: meta_data not found in model, but required to de-normalised.")
    } else{
      # else check if the dv is not in meta_data
      if (!(dv %in% meta_data$variable)) {
        # if the dv is not found in the norm table if(verbose)print warning
        ### STA could work if the de_normalise is true
        cat("\n Warning: dv not found in meta_data.")

      } else{
        pool_mean = tibble(raw_actual = raw_actual %>% pull(),
                           pool = pool %>% pull()) %>%
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
    }
  }

  # IMPROVE CATEGORIES CHECK?
  if (is.null(categories)) {
    if(all(model_table$category == "")){
      if(verbose){
        cat("Warning: no categories table provided and no categories found in model_table. Setting category_decomp = variable_decomp.\n")
      }
      category_decomp = variable_decomp
    }else{
      # create category from model table categories
      categories = model_table %>%
        select(variable,category) %>%
        mutate(category = if_else(category == '','Other',category)) %>%
        mutate(calc = 'none')
    }
  } else if(!is.data.frame(categories)){
    if(all(model_table$category == "")){
      if(verbose){
        cat("Warning: categories table provided is not a data.frame and no categories found in model_table. Setting category_decomp = variable_decomp.\n")
      }
      category_decomp = variable_decomp
    }else{

      if(verbose){
        cat("Warning: categories provided must be of type data.frame. Using model_table categories.\n")
      }
      # create category from model table categories
      categories = model_table %>%
        select(variable,category) %>%
        mutate(category = if_else(category == '','Other',category)) %>%
        mutate(calc = 'none')
    }
  }else if(!(all(c('variable','category') %in% colnames(categories)))){
    if(verbose){
      cat("Warning: categories provided must contain atleast columns 'variable' and 'category'.\n")
    }
    if(all(model_table$category == "")){
      if(verbose){
        cat("Warning: categories table provided is not a data.frame and no categories found in model_table. Setting category_decomp = variable_decomp.\n")
      }
      category_decomp = variable_decomp
    }else{

      if(verbose){
        cat("Warning: categories provided must be of type data.frame. Using model_table categories.\n")
      }
      # create category from model table categories
      categories = model_table %>%
        select(variable,category) %>%
        mutate(category = if_else(category == '','Other',category)) %>%
        mutate(calc = 'none')
    }
  }

  if(!exists('category_decomp')){

    if(!('calc' %in% colnames(categories))){
      if(verbose){
        cat("Warning: categories type data.frame provided does not include a 'calc' column. Setting all categories to 'none'.\n")
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
#' @import plotly
#' @import tidyverse
#' @export
#' @return a \code{plotly} bar chart of the model's decomposition
decomp_chart = function(model = NULL,
                        decomp_list = NULL,
                        pool = NULL,
                        colors = color_palette(),
                        variable_decomp = FALSE,
                        verbose = FALSE) {

  # Check verbose
  if(!is.logical(verbose)){
    cat("\n Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }

  # Check decomp_list , model
  if(is.null(model)){
    if(is.null(decomp_list)){
      if(verbose){
        cat("Error: no decomp_list provided. Returning NULL. \n ")
      }
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
    if(verbose){
      cat("Error: decomp table must include 3 columns called 'pool', 'variable' and 'value'. Returning NULL. \n ")
    }
    return(NULL)
  }

  # get actual dependent variable table
  fitted_values = decomp_list$fitted_values

  # check fitted_values
  if(!is.data.frame(fitted_values)){
    ##
  }
  if(!all(c("pool","variable","value") %in% colnames(fitted_values))){
    if(verbose){
      cat("\n Error: fitted_values table must include 3 columns called 'pool', 'variable' and 'value'. Returning NULL.")
    }
    return(NULL)
  }

  fitted_values = fitted_values[fitted_values$variable %in% c("actual","predicted", "residual"), ]

  # the id variable name is the first column name
  id_var = colnames(decomp)[1]


  # filter by pool if provided
  if (!is.null(pool)) {

    if(!any(decomp$pool == pool)){
      if(verbose){
        cat("\n Warning: POOL ",pool," not found. No POOL filtering applied.")
      }
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
      cat("\n Warning: no pool provided. Aggregating by id_var.")
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
      line = list(color = c("black")),
      name = "actual"
    ) %>%
    add_lines(
      data = fitted_values %>%
        filter(variable == "residual"),
      x = ~ get(id_var),
      y = ~ value,
      line = list(color = c("red")),
      name = ~ variable
    ) %>%
    layout(barmode = "relative",
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           title = title,
           font = list(color = '#1c0022'),
           xaxis = list(title = id_var,
                        showgrid = FALSE,
                        zerolinecolor = "#1c0022"))

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
                     colors = NULL) {


  # Check verbose
  if(!is.logical(verbose)){
    cat("\n Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }


  # Check decomp_list , model
  if(is.null(model)){
    if(is.null(decomp_list)){
      if(verbose){
        cat("\n Error: no decomp_list provided. Returning NULL.")
      }
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
      if(verbose){
        cat("\n Warning: POOL ",pool," not found. No POOL filtering applied.")
      }
    }
    else{
      fitted_values = fitted_values[fitted_values$pool == pool, ]
    }
  }

  if(is.null(pool)){
    if(verbose){
      cat("\n Warning: no pool provided. Aggregating by id_var.")
    }
    fitted_values = fitted_values %>%
      group_by(variable,!!sym(id_var)) %>%
      summarise(value = sum(value)) %>%
      mutate(pool = "total_pool")

    pool = "total_pool"
  }

  # colors
  if(is.null(colors)){
    c1 = "#c90f3a"
    c2 = "#1c0022"
    c3 = "#00cf74"
  }
  else{
    c1 = colors[1]
    c2 = colors[2]
    c3 = colors[3]
  }

  # plot

  plot_ly(fitted_values) %>%
    add_lines(
      data = filter(fitted_values, variable == "residual"),
      x = ~ get(id_var),
      y = ~ value,
      line = list(color = c1),
      color =  ~ variable
    ) %>%
    add_lines(
      data = filter(fitted_values, variable == "actual"),
      x = ~ get(id_var),
      y = ~ value,
      line = list(color = c2),
      color =  ~ variable
    ) %>%
    add_lines(
      data = filter(fitted_values, variable == "predicted"),
      x = ~ get(id_var),
      y = ~ value,
      line = list(color = c3),
      color =  ~ variable
    ) %>%
    layout(
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      title = 'Fit Chart',
      font = list(color = '#1c0022'),
      xaxis = list(
        showgrid = FALSE,
        zerolinecolor = "#1c0022",
        title = id_var
      )
    )

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
#' @param verbose A boolean to specify whether to print warnings
#' @import plotly
#' @import tidyverse
#' @export
#' @return a \code{plotly} histogram of the model's residuals
resid_hist_chart = function(model = NULL,
                            decomp_list = NULL,
                            pool = NULL,
                            color = "black",
                            verbose = FALSE){
  # Check verbose
  if(!is.logical(verbose)){
    cat("\n Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }


  # Check decomp_list , model
  if(is.null(model)){
    if(is.null(decomp_list)){
      if(verbose){
        cat("\n Error: no decomp_list provided. Returning NULL.")
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
    layout(font = list(color = '#1c0022'),
           title = 'Residual Distribution',
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)")

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
                                    verbose = FALSE){

  # Check verbose
  if (!is.logical(verbose)) {
    cat("\n Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }


  # Check decomp_list , model
  if (is.null(model)) {
    if (is.null(decomp_list)) {
      if (verbose) {
        cat("\n Error: no decomp_list provided. Returning NULL.")
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
      print('Warning: pool not found in data. Using full data.')
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
      font = list(color = '#1c0022'),
      title = 'Heteroskedasticity',
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
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
#' @param verbose A boolean to specify whether to print warnings
#' @import plotly
#' @import tidyverse
#' @export
#' @return a \code{plotly} bar chart of the model's ACF
acf_chart = function(model = NULL,
                     decomp_list,
                     pool = NULL,
                     color = "black",
                     verbose = FALSE){

  # Check verbose
  if (!is.logical(verbose)) {
    cat("\n Warning: verbose must be logical (TRUE or FALSE). Setting to False.")
    verbose = FALSE
  }


  # Check decomp_list , model
  if (is.null(model)) {
    if (is.null(decomp_list)) {
      if (verbose) {
        cat("\n Error: no decomp_list provided. Returning NULL.")
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
      font = list(color = '#1c0022'),
      title = 'ACF',
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      xaxis = list(
        showgrid = FALSE,
        zerolinecolor = "#1c0022")
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
#' @param trans_only a boolean specifying whether to display non-linear only \code{y = b*dim_rest(x)}
#' @param colors character vector of colors in hexadecimal notation
#' @param verbose A boolean to specify whether to print warnings
#' @param table A boolean to specify whether to return a \code{data.frame} of the response curves
#' @param points A boolean to specify whether to include the points from the data on the curve
#' @import plotly
#' @import tidyverse
#' @import tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats na.omit
#' @importFrom rlist list.append
#' @importFrom purrr reduce
#' @export
#' @return a \code{plotly} line chart of the model's response curves
#' @examples
#' run_model(data = mtcars,dv = 'mpg',ivs = c('wt','cyl','disp')) %>%
#'    response_curves()
#' run_model(data = scale(mtcars) %>% data.frame(),dv = 'mpg',ivs = c('wt','cyl','disp')) %>%
#'    response_curves()
response_curves = function(
  model,
  x_min = -100,
  x_max = 100,
  y_min = -100,
  y_max = 100,
  interval = 0.1,
  trans_only = FALSE,
  colors = color_palette(),
  verbose = FALSE,
  table = FALSE,
  points = FALSE){
  # checks  ####
  if (is.null(x_max)) x_max = 1e+05
  if (is.null(x_min)) x_min = 0
  if (is.null(interval)) interval = x_max/1000
  if (is.null(y_max)) y_max = x_max
  if (is.null(y_min)) y_min = x_min
  
  # process ####    
  optim_table = model$output_model_table
  trans_df = model$trans_df
  if (trans_only) {
    optim_table = optim_table[!(((optim_table[trans_df$name] == 
                                    "") %>% data.frame() %>% rowSums()) == nrow(trans_df)), 
    ]
  }
  optim_table = optim_table %>% filter(variable != "(Intercept)")
  optim_table = optim_table[c("variable", "variable_t", 
                              trans_df$name, "coef")] %>% na.omit()
  if (nrow(optim_table) == 0) {
    if (verbose) 
      print("All trans in model_table are blank.")
    return(plot_ly())
  }
  curves_df = list()
  x_raw = seq(x_min, x_max, interval)
  for (i in 1:nrow(optim_table)) {
    var = optim_table$variable_t[i]
    coef = optim_table$coef[i]
    x = x_raw
    for (j in 1:nrow(model$trans_df)) {
      t_name = model$trans_df$name[j]
      t_func = model$trans_df$func[j]
      param_vals = model$output_model_table %>% filter(variable_t == 
                                                         var) %>% pull(!!sym(t_name)) %>% strsplit(split = ",")
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
    curves_df = list.append(curves_df, data.frame(value = x * 
                                                    coef, variable = var, x = x_raw) %>% mutate(value = as.numeric(value)) %>% 
                              mutate(variable = as.character(variable)) %>% mutate(x = as.numeric(x)))
  }
  curves_df = curves_df %>% purrr::reduce(rbind) %>% filter(value >= 
                                                              y_min) %>% filter(value <= y_max)
  if (table) {
    return(curves_df)
  }
  
  # plotly  ####
  p = plot_ly()
  p = p %>% add_trace(data = curves_df, x = ~x, y = ~value, 
                      color = ~variable, mode = "lines", type = "scatter", 
                      colors = colors) %>% layout(plot_bgcolor = "rgba(0, 0, 0, 0)", 
                                                  paper_bgcolor = "rgba(0, 0, 0, 0)", font = list(color = "#1c0022"), 
                                                  xaxis = list(showgrid = FALSE), yaxis = list(title = model$dv), 
                                                  title = "Response Curves")
  
  if(points) {
    
    raw_data = model$model
    
    # calculate predicted points through functions
    curves_df = list()
    for (i in 1:nrow(optim_table)) {
      var = optim_table$variable_t[i]
      coef = optim_table$coef[i]
      x_raw = raw_data %>% pull(!!sym(var))
      x = x_raw
      for (j in 1:nrow(model$trans_df)) {
        t_name = model$trans_df$name[j]
        t_func = model$trans_df$func[j]
        param_vals = model$output_model_table %>% filter(variable_t == 
                                                           var) %>% pull(!!sym(t_name)) %>% strsplit(split = ",")
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
      curves_df = list.append(curves_df, data.frame(value = x * 
                                                      coef, variable = var, x = x_raw) %>% mutate(value = as.numeric(value)) %>% 
                                mutate(variable = as.character(variable)) %>% mutate(x = as.numeric(x)))
    }
    curves_df = curves_df %>% 
      purrr::reduce(rbind) #%>% 
    # filter(value >= y_min) %>% filter(value <= y_max)
    
    
    # add points to plotly item
    p = p %>%  add_trace(data = curves_df, x = ~x, y = ~value, 
                         color = ~variable, mode = "markers", type = "scatter", 
                         colors = colors)
  }
  
  return(p)
}
