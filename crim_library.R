library(tidyverse)
library(httr)
library(rvest)
library(XML)
library(xml2)
library(RCurl)

## creating URLs to scrape information from for each Boston neighborhood ## 

  neighborhoods <- c("allston","back-bay","beacon-hill", "brighton", "charlestown",
                   "dorchester","downtown", "east-boston", "fenway", "hyde-park", "jamaica-plain", 
                   "mattapan", "mission-hill", "north-end", "roslindale", "roxbury", "south-boston", 
                   "south-end", "west-roxbury") 

  neighborhoods_URL <- paste("https://www.universalhub.com/crime/" ,neighborhoods, ".html", sep = "")

  #special case dorchester has second page! 
  neighborhoods_URL <- c(neighborhoods_URL, paste("https://www.universalhub.com/crime/" ,neighborhoods[6], 
                                                ".html?page=1", sep = ""))
  
  neighborhoods_URL <- neighborhoods_URL[c(1:6, 20, 7:19)]
  
  neighborhoods <- c("allston","back-bay","beacon-hill", "brighton", "charlestown",
                      "dorchester","dorchester","downtown", "east-boston", "fenway", "hyde-park", "jamaica-plain", 
                      "mattapan", "mission-hill", "north-end", "roslindale", "roxbury", "south-boston", 
                      "south-end", "west-roxbury")
  
  #special case mission hill does not have .html at the end 
  neighborhoods_URL[14] <- gsub(".html", "", neighborhoods_URL[14])


##Scraping Functions## 
  
get_site_content <- function(urls){
  content <- list() #create empty list to store all htmls 
  require( httr )
  #loop to extract content from each url
  for(i in 1:length(urls)){
  response <- httr::GET(urls[i])
  # extract the content
  content[[i]] <- httr::content(x = response, as = 'text', encoding = 'utf-8' )
  }
  return(content)
}

content_to_parsed_html <- function(contents){
  parsed_htmls <- list() #create a list to store all parsed htmls 
  require(xml2)
  # loop to parse all htmls with xml2
  for(i in 1:length(contents)){
    parsed_htmls[[i]] <- xml2::read_html(contents[[i]])
  }
  # return
  return(parsed_htmls)
}

parse_crimes_dates <- function(parsed_htmls){
  crimes_dates <- vector("list", length = 20)
  names(crimes_dates) <- neighborhoods
  require(rvest)
  #for loop to extract dates from each html
  for(i in 1:length(parsed_htmls)){
    # extract the tbody element(s)
      #extract crime types
      crime <- rvest::html_nodes( x = parsed_htmls[[i]], xpath = "//td[contains(@class, 'field-name')]")
      crime <- rvest::html_text(crime)
      #extract dates 
      date  <- rvest::html_nodes( x = parsed_htmls[[i]], xpath = "//td[contains(@class, 'crime-date')]/span")
      date  <- rvest::html_text(date)
      #data.frame for dates and crimes 
      crimes_dates[[i]] <- data.frame(crime,date)
  }
  return(crimes_dates)
}


cleaning_function <- function(data){
  for(i in 1:length(data)){
    data[[i]][[1]] <- as.character(data[[i]][[1]])
    data[[i]][[2]] <- as.character(data[[i]][[2]])
  }
  return(data)
  }



ATEST <- get_site_content(neighborhoods_URL)
ATEST2 <- content_to_parsed_html(ATEST)
ATEST3 <- parse_crimes_dates(ATEST2)
