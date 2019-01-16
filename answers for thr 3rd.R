library(tidyverse)
library(xml2)
library(RCurl)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


#' Question 1: Get Population Ranking
#'
#' @return
#' @export
#'
#' @examples
get_population_ranking <- function(){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url = str_c(base_url, "fields/335rank.html")
  #download url and execute all XPath queries which will each return a column for a data_frame
  raw_html <- read_html(getURL(url, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  
  tibble <- lapply(x = raw_html, xpath_expressions, xml_find_all) %>% 
    map(function(x) as_list(x)) %>% 
    map_df(function(x) unlist(x)) 
  
  #make the necessary adjustments to the data frame as given by the assignment
  tibble$country_link <- str_replace_all(tibble$country_link, "^../", "")
  tibble <- rename(tibble, population = value)
  tibble <- rename(tibble, rank.population = rank)
}

first_answer <- get_population_ranking()


#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#'
#' @return
#' @export
#'
#' @examples
get_land_area <- function(country_link){
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  #download the file from country_link and execute the xpath query
  url2 <- str_c(base_url, country_link)
  raw_html2 <- lapply(url2, getURL, .encoding = "UTF-8", .opts = list(followlocation = FALSE)) %>% map(read_html)
  unlisted <- raw_html2 %>% lapply(xml_find_all, xpath) %>% map(~as_list(.)) %>% map_chr(~unlist(.))
}

# It works when tried for a c() having 2 urls (which is called as 'vector' below).

vector <- c("geos/et.html","geos/tu.html")
trial<- get_land_area(vector)

# It also worked when I tried for the full vector of 238 countries with first_answer$country_link, as shown below.
# It took 2-3 minutes in my laptop. But gives the right and complete dataframe. So, please wait for it. 

second_answer<- get_land_area(first_answer$country_link)


#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  combined <- cbind(first_answer, second_answer)
  
  combined <- rename(combined, land_area = second_answer)
  combined$land_area <- gsub("[^0-9\\]", "", combined$land_area)
  combined$population <- gsub("[^0-9\\]", "", combined$population) 
  combined[12, "land_area"] <- 1000000
  
  combined$land_area <- as.numeric(combined$land_area)
  combined$population <- as.numeric(combined$population)
  
  combined <- mutate(combined, population_density = population / land_area)
  
}

# Since there is "second_answer" in the function, final answer (second_answer<- get_land_area(first_answer$country_link) from the previous question 
# has to be run before this one.

third_answer <- get_population_density()


#' Question 4: Get All Provided Rankings
#'
#' @return
#' @export
#'
#' @examples
get_rankings <- function(){
  url4 <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath4 <- c("characteristic" = "//div[@class='field_label']/strong/a",
              "characteristic_link" = "//div[@class='field_label']/strong/a/@href")
  raw_html4 <- read_html(getURL(url4, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  
  tibble4 <- lapply(x = raw_html4, xpath4, xml_find_all) %>% 
    map(function(x) as_list(x)) %>% 
    map_df(function(x) unlist(x)) 
  
  #making the necessary adjustments to the data frame as given by the assignment
  tibble4$characteristic_link <- str_replace_all(tibble4$characteristic_link, "^../", "")
  tibble4$characteristic <- str_replace_all(tibble4$characteristic, ":", "")
  tibble4$characteristic <- tolower(tibble4$characteristic)
  return(tibble4)
  View(tibble4) 
}

fourth_answer <- get_rankings()



#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return
#' @export
#'
#' @examples
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  
  url5 <- str_c(base_url, url)
  raw_html5 <- read_html(getURL(url5, .encoding = "UTF-8", .opts = list(followlocation = FALSE)))
  
  tibble5 <- lapply(x = raw_html5, xpath_expressions, xml_find_all) %>% 
    map(function(x) as_list(x)) %>% 
    map_df(function(x) unlist(x)) 
  
  #making the necessary adjustments to the data frame as given by the assignment
  tibble5$country_link <- str_replace_all(tibble5$country_link, "^../", "")
  tibble5 <- rename(tibble5, !!characteristic:= value)
}

#now the function's output depends on the url and characteristics we write into it.

#example 
example5a <- get_ranking("fields/345rank.html", "birth rate")


#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return
#' @export
#'
#' @examples
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  #update the xpath and use similar code other than that
  xpath <- str_c("//div[@id='", xpath_field_id ,"']/div[", item ,"]/span[2]")
  
  #download the file from country_link and execute the xpath query
  url5b <- str_c(base_url, country_link)
  raw_html2 <- lapply(url5b, getURL, .encoding = "UTF-8", .opts = list(followlocation = FALSE)) %>% map(read_html)
  unlisted <- raw_html2 %>% lapply(xml_find_all, xpath) %>% map(~as_list(.)) %>% map_chr(~unlist(.))
  
}

#example 
example5b <- get_country_characteristic("geos/us.html", "field-area", 2)


#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  #using map2 to iterate over both characteristic_link and characteristic columns
  ranks <- map2(rankings$characteristic_link, rankings$characteristic, get_ranking) 
  ranks2 <- lapply(ranks, `length<-`, max(lengths(ranks)))
  ranks3 <- ranks2 %>% reduce(full_join, by=c("country_link","country"))
}

# It is slower than previous answers, but works. 

View(combine_rankings(fourth_answer))
sixth_answer <- combine_rankings(fourth_answer)
