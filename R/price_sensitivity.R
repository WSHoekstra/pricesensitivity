#' @import haven
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import plotly


determine_n_values_under_cutoff <- function(cutoffs, values){
  #`  Given a vector of cutoffs and a vector of values, return the amount of values that are under that cutoff
  results <- c()
  for(i in cutoffs) {
    result <- sum(values < i)
    results <- c(results, result)
  }
  return(results)
}



determine_n_values_over_cutoff <- function(cutoffs, values){
  #`  Given a vector of cutoffs and a vector of values, return the amount of values that are over that cutoff
  results <- c()
  for(i in cutoffs) {
    result <- sum(values > i)
    results <- c(results, result)
  }
  return(results)
}


generate_pricing_stats <- function(surveydata,
              too_expensive,
              bargain,
              getting_expensive,
              too_cheap){
  #` Given survey data that conforms to the van Westerdorp methodology
  #` (4 questions about product pricing:
  # `  1: pick the max price point at which the product would be too cheap
  # `  2: pick the min price point at which the product would be too expensive
  # `  3: pick the max price point at which the product would be close to too expensive, but still acceptable
  # `  4: pick the max price point at which the product would be a bargain

  #' Generate a dataframe of pricing statistics
  #' over a sliding window from the minimum amount to
  #' the maximum.
  surveydata <- surveydata[c(too_expensive,
                     bargain,
                     getting_expensive,
                     too_cheap)]
  min_val <- floor(min(surveydata)) - 1
  max_val <- ceiling(max(surveydata)) + 1
  n <- nrow(surveydata)
  price_range <- c(min_val:max_val)

  # goes up
  too_expensive_vector <- round( (determine_n_values_under_cutoff(values = surveydata[too_expensive], cutoffs = price_range)  / n),3)
  getting_expensive_vector <- round( (determine_n_values_under_cutoff(values = surveydata[getting_expensive], cutoffs = price_range)  / n),3)

  # goes down
  too_cheap_vector <- round( (determine_n_values_over_cutoff(values = surveydata[too_cheap], cutoffs = price_range)  / n),3)
  bargain_vector <- round( (determine_n_values_over_cutoff(values = surveydata[bargain], cutoffs = price_range)  / n),3)

  pricing_stats <- data.frame('price' = price_range,
                       'too_cheap' = too_cheap_vector,
                       'too_expensive' = too_expensive_vector,
                       'bargain' = bargain_vector,
                       'getting_expensive' = getting_expensive_vector)

  pricing_stats$lower_bound_met <- (pricing_stats$too_cheap < pricing_stats$getting_expensive)
  pricing_stats$upper_bound_met <- (pricing_stats$bargain < pricing_stats$too_expensive)

  # lower bound
  lower_bound_x1 <- max(pricing_stats$price[pricing_stats$lower_bound_met == FALSE])

  too_cheap_where_lower_bound_last_not_met <- min(pricing_stats$too_cheap[pricing_stats$lower_bound_met == FALSE]) # y1 [1]
  too_cheap_where_lower_bound_first_met <- max(pricing_stats$too_cheap[pricing_stats$lower_bound_met == TRUE]) # y1 [2]

  getting_expensive_where_lower_bound_last_not_met <- max(pricing_stats$getting_expensive[pricing_stats$lower_bound_met == FALSE]) # y2 [1]
  getting_expensive_where_lower_bound_first_met <- min(pricing_stats$getting_expensive[pricing_stats$lower_bound_met == TRUE]) # y2 [2]
  too_cheap_slope = too_cheap_where_lower_bound_first_met - too_cheap_where_lower_bound_last_not_met
  getting_expensive_slope = getting_expensive_where_lower_bound_first_met - getting_expensive_where_lower_bound_last_not_met
  delta_y_lowerbound = too_cheap_where_lower_bound_last_not_met - getting_expensive_where_lower_bound_last_not_met
  lower_bound =  lower_bound_x1 + (delta_y_lowerbound / (too_cheap_slope + getting_expensive_slope))

  # upper bound
  upper_bound_x1 <- max(pricing_stats$price[pricing_stats$upper_bound_met == FALSE])

  bargain_where_upper_bound_last_not_met <- min(pricing_stats$bargain[pricing_stats$upper_bound_met == FALSE])
  bargain_where_upper_bound_first_met <- max(pricing_stats$bargain[pricing_stats$upper_bound_met == TRUE])

  too_expensive_where_upper_bound_last_not_met <- max(pricing_stats$too_expensive[pricing_stats$upper_bound_met == FALSE])
  too_expensive_where_upper_bound_first_met <- min(pricing_stats$too_expensive[pricing_stats$upper_bound_met == TRUE])

  bargain_slope =  bargain_where_upper_bound_first_met - bargain_where_upper_bound_last_not_met
  too_expensive_slope = too_expensive_where_upper_bound_first_met - too_expensive_where_upper_bound_last_not_met
  delta_y_upperbound =  too_expensive_where_upper_bound_last_not_met - bargain_where_upper_bound_last_not_met
  #
  upper_bound =  upper_bound_x1 + (delta_y_upperbound / (bargain_slope + too_expensive_slope))

  metadata <- list('n' = n,
                   'lower_bound' = lower_bound,
                   'upper_bound' = upper_bound)

  return(list('data' = pricing_stats,
              'metadata' = metadata))
}




find_price_optimimum_range <- function(pricing_stats){
  #` Find the optima cutoffs in the pricing stats
  lower_bound <- min(pricing_stats$price[pricing_stats$lower_bound_met == TRUE])
  upper_bound <- min(pricing_stats$price[pricing_stats$upper_bound_met == TRUE])
  return( c(lower_bound, upper_bound))
}

visualize_pricing_stats <- function(pricing_stats,
                        pricing_stats_metadata,
                        title,
                        path=NULL,
                        line_size = 1.5,
                        add_price_range_to_title = TRUE,
                        add_n_to_title = TRUE,
                        title_orientation = 'center',
                        too_cheap_color = "#60BD68",
                        too_expensive_color = "#F15854",
                        bargain_color ="#5DA5DA",
                        getting_expensive_color="#B276B2",
                        currency = ''
                        ){
  #` Visualizes pricing stats as generated by the generate_pricing_stats function.
  lower_bound <- pricing_stats_metadata$lower_bound
  upper_bound <- pricing_stats_metadata$upper_bound
  ylim_max <-  upper_bound + lower_bound
  n <- pricing_stats_metadata$n

  if (currency == 'dollars') {
    currency <- '$'
  }

  else if (currency == 'euros') {
    currency <- '€'
  }

  else {

    currency <- ''
  }

  if (add_price_range_to_title)
  {
    title <- paste0(title, '\n', 'optimum between ', currency, formatC(round(lower_bound,2), digits = 2, format = "f"), ' and ', currency, formatC(round(upper_bound,2), digits = 2, format = "f"))
  }

  if (add_n_to_title)
  {
    title <- paste0(title, '\n', 'based on data from ', n, ' respondents')
  }

  p <- ggplot2::ggplot(data = pricing_stats %>% dplyr::filter(price <= ylim_max), ggplot2::aes(x = price))+
    ggplot2::theme_bw() +
    ggplot2::geom_line(aes(y = too_cheap, color = "too_cheap"), size=line_size) +
    ggplot2::geom_line(aes(y = too_expensive, color = "too_expensive"), size=line_size) +
    ggplot2::geom_line(aes(y = bargain, color = "bargain"), size=line_size) +
    ggplot2::geom_line(aes(y = getting_expensive, color = "getting_expensive"), size=line_size) +
    ggplot2::geom_vline(xintercept = lower_bound, linetype = "dotted", size=1) +
    ggplot2::geom_vline(xintercept = upper_bound, linetype = "dotted", size=1) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(axis.title.x = ggplot2::element_text('Price'),
          axis.title.y = ggplot2::element_blank()) +
    ggplot2::scale_colour_manual(name = "Legend",
                        values=c(too_cheap = too_cheap_color,
                                 too_expensive = too_expensive_color,
                                 bargain = bargain_color,
                                 getting_expensive = getting_expensive_color))

  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)

    if (title_orientation == 'center')
      {
      p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      }
  }
  return(p)
}



