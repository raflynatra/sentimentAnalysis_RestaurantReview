library(rvest)

baseUrl <- "https://www.tripadvisor.com"

get_restaurant <- function(cityURL){
  
  url <- paste(baseUrl, cityURL, sep = "")
  webpage <- read_html(url)
  
  restoName <- webpage %>% html_nodes('.wQjYiB7z') %>% html_text()
  restoURL <- webpage %>% html_nodes('._15_ydu6b') %>% html_attr('href')
  
  resto <- data.frame(name = restoName, link = restoURL, stringsAsFactors = FALSE)

}