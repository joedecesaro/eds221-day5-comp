# testing testthat

library(testthat)
library(devtools)
library(remotes)

# create a function that takes any nnumeric data frame (all variables are numbers), finds the mean of all columns, then finds the maximum column mean, and the minimum column mean from that data frame, & returns those min and max values in a vector (min_mean_val, max_mean_val)


#' Column Minimum and Maximum Mean
#' 
#' Description: The function returns a data frame's column minimum mean and column maximum mean for all columns.
#'
#' @param df Any data frame
#'
#' @return
#' @export
#'
#' @examples
mean_range <- function(df) {
  col_means <- apply(X = df, MARGIN = 2, FUN = mean, na.rm = TRUE)
  col_mean_max <- max(col_means)
  col_mean_min <- min(col_means)
  return(c(col_mean_min, col_mean_max))
}

mean_range(df = mtcars)

expect_length(mean_range(df = mtcars), 3)

expect_true(mean_range(df = mtcars)[1] < mean_range(df = mtcars)[2])

expect_s3_class(mean_range(mtcars), "vector")

expect_type(mean_range(mtcars), "double")


