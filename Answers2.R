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


# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}

get_jane_austen_data()

# extract_possible_names 

#' Extracts all words that start with a capital letter from texts
#'
#' @param data dataframe  
#'
#' @return a data frame with three columns; “text_id” a unique identifier, “id” from the original data frame
#' and “name”, extracted name for all rows which are extracted names.

extract_possible_names <- function(data){
  extracted <- data
  
  extracted <- data.frame(text = gsub('[[:punct:] ]+',' ',extracted$text),
                          id = extracted$id, title= extracted$title)
  
  extracted <- mutate(extracted, 
                      name = unlist(sapply(
                        str_extract_all(extracted$text, '\\b[A-Z]\\w+'), paste, collapse = ' ')))
  
  extracted <- extracted[!(is.na(extracted$name) | extracted$name==""), ]
  
  s <- strsplit(extracted$name, split = " ")
  extracted <- data.frame(text_id = 
                            rep(extracted$id, sapply(s, length)), name = unlist(s))
  
  extracted <- mutate(extracted, id = 1:nrow(extracted))
  
} 

answer2 <- extract_possible_names(austen_text)  


# Question 3 ------------------------------------------------------------------------------------------------------

# I am calling Freqs;

Freqs <- readRDS("austen_word_freqs.Rds")

colnames(Freqs) <- c("Var1", "Freq")

# I am writing a function to display the rates neatly, later on in the next function.

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# filter_names / I am counting the Capitaliazed words, "Freqs" is right joining to those, 
# first filtering never Capitalized words, calculating the rates and eliminating those with less than 75% rate,
# displaying words and their percentages, they are capitalized at least 75% of the time.

#' Filters out non-name words 
#'
#' @param data dataframe
#'
#' @return a data frame with two columns; "Var1" filtered names, "percentage' for their rates,
#' first letters of all words are lower case automatically

filter_names <- function(data) {
  capitalized <- data
  
  capitalized <- data.frame(table(unlist(strsplit(tolower(capitalized$name), " ")))) %>% 
    right_join(Freqs, by = "Var1")
  
  capitalized <- capitalized[!(is.na(capitalized$Freq.x) | capitalized$Freq.x==""), ] %>% 
    mutate(rate = Freq.x / Freq.y)
  
  capitalized <- capitalized[!(capitalized$rate <= 0.75),] 
  
  capitalized <- mutate(capitalized, percentage = percent(capitalized$rate))
  
  capitalized <- capitalized %>%
    select(Var1, percentage)
}


# I am calling the answer (dataframe) of the previous question to work on it. 

extracted <- extract_possible_names(austen_text)

answer3 <- filter_names(extracted)

# I tried to use pipes, however, at most of the points, they were creating problems, thus, 
# I have deleted them, I am sorry for the repetetive "capitalized" mess.


