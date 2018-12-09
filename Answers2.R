library(tidyverse)


# Question 1 ------------------------------------------------------------------------------------------------------

#' Tidies data frame by gathering all columns that start with a given column prefix.
#'
#' @param data 
#' @param column_prefix 
#'
#' @return a tidied dataframe

tidy_df <- function(data, column_prefix = "var"){
  a <- str_subset(names(data), "^var")
  gather(data, a, key = "variable", value = "value")
}

# I used "str_subset" to select the elements that are matching the patterns, 
# "names" to get the column names and "gather" to gather those columns into a new pair of variables.
# Columns that do not start with the given prefix are left.

#' @examples
# Above function I created works when I try this example below;

names <- c('John Doe','Peter Gynn','Jolie Hope')
var1 <- c(21000, 23400, 26800)
var2 <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))

employ.data <- data.frame(names, var1, var2)

example_answer1 <- tidy_df(employ.data, "var")
